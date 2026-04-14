import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import * as https from "https";
import * as http from "http";
import { execSync } from "child_process";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

const REPO = "tree-sitter-perl/perl-tree-sitter-lsp";
const VERSION = "0.2.4";

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
  const serverPath = await ensureBinary(context);
  if (!serverPath) {
    return;
  }

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "perl" }],
    outputChannelName: "perl-lsp",
  };

  client = new LanguageClient(
    "perl-lsp",
    "Perl LSP",
    serverOptions,
    clientOptions
  );

  await client.start();
}

export async function deactivate() {
  if (client) {
    await client.stop();
  }
}

// ---- Binary management ----

async function ensureBinary(
  context: vscode.ExtensionContext
): Promise<string | undefined> {
  // 1. User-configured path takes priority
  const configPath = vscode.workspace
    .getConfiguration("perl-lsp")
    .get<string>("path");
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
    const result = execSync(`${which} perl-lsp`, { encoding: "utf8" }).trim();
    if (result) {
      return "perl-lsp";
    }
  } catch {
    // not on PATH, proceed to download
  }

  // 4. Download from GitHub Releases
  return downloadBinary(storagePath, cachedBinary);
}

const PLATFORM_MAP: Record<string, string> = {
  "linux-x64": "perl-lsp-x86_64-unknown-linux-gnu.tar.gz",
  "darwin-x64": "perl-lsp-x86_64-apple-darwin.tar.gz",
  "darwin-arm64": "perl-lsp-aarch64-apple-darwin.tar.gz",
  "win32-x64": "perl-lsp-x86_64-pc-windows-msvc.zip",
};

async function downloadBinary(
  storagePath: string,
  targetPath: string
): Promise<string | undefined> {
  const key = `${process.platform}-${process.arch}`;
  const asset = PLATFORM_MAP[key];

  if (!asset) {
    vscode.window.showErrorMessage(
      `perl-lsp: no pre-built binary for ${key}. Install manually: cargo install perl-lsp`
    );
    return undefined;
  }

  const url = `https://github.com/${REPO}/releases/download/v${VERSION}/${asset}`;

  return vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: "perl-lsp: downloading language server...",
      cancellable: false,
    },
    async () => {
      try {
        const dir = path.dirname(targetPath);
        fs.mkdirSync(dir, { recursive: true });

        const archivePath = path.join(dir, asset);
        await download(url, archivePath);

        // Extract
        if (asset.endsWith(".tar.gz")) {
          execSync(`tar xzf "${archivePath}" -C "${dir}"`);
        } else if (asset.endsWith(".zip")) {
          execSync(`powershell -Command "Expand-Archive -Path '${archivePath}' -DestinationPath '${dir}'"`)
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

        vscode.window.showErrorMessage(
          "perl-lsp: download succeeded but binary not found after extraction"
        );
        return undefined;
      } catch (err: any) {
        vscode.window.showErrorMessage(
          `perl-lsp: failed to download — ${err.message}. Install manually: cargo install perl-lsp`
        );
        return undefined;
      }
    }
  );
}

function download(url: string, dest: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(dest);
    const get = url.startsWith("https") ? https.get : http.get;

    get(url, (response) => {
      // Follow redirects (GitHub releases use 302)
      if (
        response.statusCode &&
        response.statusCode >= 300 &&
        response.statusCode < 400 &&
        response.headers.location
      ) {
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
