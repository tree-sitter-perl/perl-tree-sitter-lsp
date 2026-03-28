"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const path = __importStar(require("path"));
const fs = __importStar(require("fs"));
const https = __importStar(require("https"));
const http = __importStar(require("http"));
const child_process_1 = require("child_process");
const node_1 = require("vscode-languageclient/node");
const REPO = "tree-sitter-perl/perl-tree-sitter-lsp";
const VERSION = "0.2.0";
let client;
async function activate(context) {
    const serverPath = await ensureBinary(context);
    if (!serverPath) {
        return;
    }
    const serverOptions = {
        command: serverPath,
        args: [],
    };
    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "perl" }],
        outputChannelName: "perl-lsp",
    };
    client = new node_1.LanguageClient("perl-lsp", "Perl LSP", serverOptions, clientOptions);
    await client.start();
}
async function deactivate() {
    if (client) {
        await client.stop();
    }
}
// ---- Binary management ----
async function ensureBinary(context) {
    // 1. User-configured path takes priority
    const configPath = vscode.workspace
        .getConfiguration("perl-lsp")
        .get("path");
    if (configPath && fs.existsSync(configPath)) {
        return configPath;
    }
    // 2. Check cached binary in globalStorage
    const storagePath = context.globalStorageUri.fsPath;
    const binaryName = process.platform === "win32" ? "perl-lsp.exe" : "perl-lsp";
    const cachedBinary = path.join(storagePath, VERSION, binaryName);
    if (fs.existsSync(cachedBinary)) {
        return cachedBinary;
    }
    // 3. Check if perl-lsp is on PATH
    try {
        const which = process.platform === "win32" ? "where" : "which";
        const result = (0, child_process_1.execSync)(`${which} perl-lsp`, { encoding: "utf8" }).trim();
        if (result) {
            return "perl-lsp";
        }
    }
    catch {
        // not on PATH, proceed to download
    }
    // 4. Download from GitHub Releases
    return downloadBinary(storagePath, cachedBinary);
}
const PLATFORM_MAP = {
    "linux-x64": "perl-lsp-x86_64-unknown-linux-gnu.tar.gz",
    "darwin-x64": "perl-lsp-x86_64-apple-darwin.tar.gz",
    "darwin-arm64": "perl-lsp-aarch64-apple-darwin.tar.gz",
    "win32-x64": "perl-lsp-x86_64-pc-windows-msvc.zip",
};
async function downloadBinary(storagePath, targetPath) {
    const key = `${process.platform}-${process.arch}`;
    const asset = PLATFORM_MAP[key];
    if (!asset) {
        vscode.window.showErrorMessage(`perl-lsp: no pre-built binary for ${key}. Install manually: cargo install perl-lsp`);
        return undefined;
    }
    const url = `https://github.com/${REPO}/releases/download/v${VERSION}/${asset}`;
    return vscode.window.withProgress({
        location: vscode.ProgressLocation.Notification,
        title: "perl-lsp: downloading language server...",
        cancellable: false,
    }, async () => {
        try {
            const dir = path.dirname(targetPath);
            fs.mkdirSync(dir, { recursive: true });
            const archivePath = path.join(dir, asset);
            await download(url, archivePath);
            // Extract
            if (asset.endsWith(".tar.gz")) {
                (0, child_process_1.execSync)(`tar xzf "${archivePath}" -C "${dir}"`);
            }
            else if (asset.endsWith(".zip")) {
                (0, child_process_1.execSync)(`powershell -Command "Expand-Archive -Path '${archivePath}' -DestinationPath '${dir}'"`);
            }
            // Clean up archive
            fs.unlinkSync(archivePath);
            // Mark executable on unix
            if (process.platform !== "win32") {
                fs.chmodSync(targetPath, 0o755);
            }
            if (fs.existsSync(targetPath)) {
                return targetPath;
            }
            vscode.window.showErrorMessage("perl-lsp: download succeeded but binary not found after extraction");
            return undefined;
        }
        catch (err) {
            vscode.window.showErrorMessage(`perl-lsp: failed to download — ${err.message}. Install manually: cargo install perl-lsp`);
            return undefined;
        }
    });
}
function download(url, dest) {
    return new Promise((resolve, reject) => {
        const file = fs.createWriteStream(dest);
        const get = url.startsWith("https") ? https.get : http.get;
        get(url, (response) => {
            // Follow redirects (GitHub releases use 302)
            if (response.statusCode &&
                response.statusCode >= 300 &&
                response.statusCode < 400 &&
                response.headers.location) {
                file.close();
                fs.unlinkSync(dest);
                download(response.headers.location, dest).then(resolve, reject);
                return;
            }
            if (response.statusCode !== 200) {
                file.close();
                fs.unlinkSync(dest);
                reject(new Error(`HTTP ${response.statusCode}`));
                return;
            }
            response.pipe(file);
            file.on("finish", () => {
                file.close();
                resolve();
            });
        }).on("error", (err) => {
            file.close();
            fs.unlinkSync(dest);
            reject(err);
        });
    });
}
//# sourceMappingURL=extension.js.map