import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('sanna');
    const serverPath = config.get<string>('serverPath', 'sanna');

    // Server options - run the Sanna LSP server
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            args: ['lsp'],
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            args: ['lsp'],
            transport: TransportKind.stdio
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'sanna' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.sanna')
        },
        outputChannelName: 'Sanna Language Server'
    };

    // Create the language client
    client = new LanguageClient(
        'sanna',
        'Sanna Language Server',
        serverOptions,
        clientOptions
    );

    // Register commands
    context.subscriptions.push(
        vscode.commands.registerCommand('sanna.verify', async () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'sanna') {
                await client?.sendRequest('workspace/executeCommand', {
                    command: 'sanna.verify',
                    arguments: [editor.document.uri.toString()]
                });
            }
        }),

        vscode.commands.registerCommand('sanna.approve', async () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'sanna') {
                await client?.sendRequest('workspace/executeCommand', {
                    command: 'sanna.approve',
                    arguments: [editor.document.uri.toString()]
                });
                vscode.window.showInformationMessage('Specification approved');
            }
        }),

        vscode.commands.registerCommand('sanna.review', async () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'sanna') {
                await client?.sendRequest('workspace/executeCommand', {
                    command: 'sanna.review',
                    arguments: [editor.document.uri.toString()]
                });
                vscode.window.showInformationMessage('Review requested');
            }
        }),

        vscode.commands.registerCommand('sanna.trust', async () => {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'sanna') {
                const result = await client?.sendRequest('workspace/executeCommand', {
                    command: 'sanna.trust',
                    arguments: [editor.document.uri.toString()]
                });
                vscode.window.showInformationMessage(result as string || 'Trust score calculated');
            }
        })
    );

    // Start the client
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
