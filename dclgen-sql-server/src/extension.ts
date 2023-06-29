// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { LocalStorageService } from './localStorageService';


// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "dclgen-sql-server" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	const disposable = vscode.commands.registerCommand('extension.dclgen-sql-server', async () => {
		// The code you place here will be executed every time your command is executed
		// Display a message box to the user
		
		const connections = getConnections();
		let connection;
		if (connections?.length === 0) {
			connection = await addNewConnection();
		}
		else {
			const connectionName = await vscode.window.showQuickPick([...connections.map((c: any) => c.name), "+ Add new connection"]);
			if (connectionName === "+ Add new connection") {
				connection = await addNewConnection();
			}
			else{
				connection = connections.find(c => c.name === connectionName);
			}
		}

		if (connection) {
			await generateTablesFile(connection.dbconfig);
		}


	});

	context.subscriptions.push(disposable);

	async function addNewConnection() : Promise<Config | undefined> {
		const server = await vscode.window.showInputBox({
			prompt: 'Enter server name or address',
			placeHolder: 'localhost',
		});

		if (!server) { return undefined; }

		const database = await vscode.window.showInputBox({
			prompt: 'Enter database name',
			placeHolder: '',
		});

		if (!database) { return undefined; }

		let isTrusted = await vscode.window.showInputBox({
			prompt: 'Use trusted connection (y/n)',
			placeHolder: '',
		});

		while (isTrusted?.toLocaleLowerCase() !== 'y' && isTrusted?.toLocaleLowerCase() !== 'n') {
			isTrusted = await vscode.window.showInputBox({
				prompt: 'Use trusted connection (y/n)',
				placeHolder: '',
			});

			if (!isTrusted) {
				return undefined;
			}
		}

		let savePassword;

		const config: any = {
			server,
			database,
				options: {
				enableArithAbort: true,
				encrypt: true, // for azure
				trustServerCertificate: true // change to true for local dev / self-signed certs
			},

		};

		if (isTrusted === 'n') {
			const user = await vscode.window.showInputBox({
				prompt: 'Enter user name',
				placeHolder: '',
			});

			if (!user) { return undefined; }

			config.user = user;

			const keepPassword = await vscode.window.showInputBox({
				prompt: 'Save password (Y/n)',
				placeHolder: 'y',
			});

			if (!keepPassword) { 
				savePassword = true; 
			}
			else {
				savePassword = keepPassword?.toLocaleLowerCase() === 'y';
			}

			if (savePassword) {
				const password = await vscode.window.showInputBox({
					prompt: 'Enter password',
					placeHolder: '',
				});

				config.password = password;
			}

		}
		else {
			// config.options.trustedConnection = true;
			config.integratedSecurity = true;
			savePassword = false;
		}

		const name = await vscode.window.showInputBox({
			prompt: 'Enter connection name',
			placeHolder: '',
		});


		if (!name) { 
			vscode.window.showWarningMessage("Can't create connection without a name");
			return undefined;
		}

		let conns = getConnections();

		const newconn: Config = {
			name,
			savePassword,
			dbconfig: config
		};

		conns.push(newconn);
		saveConnectionStrings(conns);

		return newconn;

	}

	async function generateTablesFile(config: any) {
		const sql = require('mssql');

		const pool = await sql.connect(config);
		// vscode.window.showInformationMessage('after pool!');

		const tables = await vscode.window.showQuickPick(getTableNames(pool), {
			placeHolder: 'Select a table / some tables',
			canPickMany: true
		});

		if (!tables) {
			vscode.window.showInformationMessage('No tables selected');
			return;
		}

		tables.forEach(async (table) => {
			await generateTableFile(pool, table);
		});
	}
	async function getTableNames(pool: any) {
		const result = await pool.request().query(`
			SELECT TABLE_NAME
			FROM INFORMATION_SCHEMA.TABLES
			WHERE TABLE_TYPE = 'BASE TABLE'
			AND TABLE_SCHEMA = 'dbo';`);

		return result.recordset.map((row: any) => row.TABLE_NAME);
	}

	async function generateTableFile(pool: any, table: string) {
		const path = require('path');

		const workspaceFolders = vscode.workspace.workspaceFolders;
		if (!workspaceFolders || workspaceFolders.length === 0) {
			vscode.window.showErrorMessage('No workspace folder found.');
			return;
		}

		const fileName = `${table}.COP`;
		const workspaceFolderPath = workspaceFolders[0].uri.fsPath;
		const filePath = path.join(workspaceFolderPath, fileName);
		// const filePath = vscode.workspace.asRelativePath(fileName);
		const fileUri = vscode.Uri.file(filePath);

		const tableFields = await retrieveTableFields(pool, table);

		const content = fillDCLGEN(table, tableFields);
		await vscode.workspace.fs.writeFile(fileUri, Buffer.from(content));
	}

	async function retrieveTableFields(pool: any, tableName: string) {
		const result = await pool.request().query(`
			SELECT COLUMN_NAME, IS_NULLABLE, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, NUMERIC_PRECISION_RADIX 
			FROM INFORMATION_SCHEMA.COLUMNS  
			WHERE TABLE_NAME = '${tableName}' 
			AND TABLE_SCHEMA = 'dbo'
			ORDER BY ORDINAL_POSITION;
		`);

		const fields = result.recordset; //.map((row: any) => row.COLUMN_NAME);
		return fields;
	}

	function fillDCLGEN(table: string, tableFields: any): string {
		let anyFieldNotNull = false;


		let dclgen = " ".repeat(9) + "01 DCL-" + table.toUpperCase() + ".\n";

		let dclgenNulls = " ".repeat(9) + "01 DCL-" + table.toUpperCase() + "-NULL.\n";

		tableFields.forEach((field: any) => {
			dclgen += " ".repeat(12) + "03 " + field.COLUMN_NAME.toUpperCase() + " ";
			switch (field.DATA_TYPE) {
				case 'int':
				case 'bit':					
					dclgen += "PIC 9(9) COMP-5";
					break;
				case 'nvarchar':
				case 'varchar':
					dclgen += "SQL TYPE IS CHAR (" + field.CHARACTER_MAXIMUM_LENGTH.toString() + ")";
					break;
				case 'datetime':
				case 'datetime2':
					dclgen += "PIC X(29)";
					break;
				case 'float':
					dclgen += "PIC S9(13).99";
			}

			dclgen += '.\n';

			if (field.IS_NULLABLE === 'YES') {
				anyFieldNotNull = true;
				dclgenNulls += " ".repeat(12) + "03 " + field.COLUMN_NAME.toUpperCase() + "NULL PIC 9(4) COMP-5.\n";
			}

		});

		if (anyFieldNotNull) {
			dclgen += '\n\n\n' + dclgenNulls;
		}

		return dclgen;
	}

	function getConnections() : Config[] {
		const storageManager = new LocalStorageService(context.globalState);

		const storedConnections = storageManager.getValue<Config[]>('dclgen.sqlserver.connectionStrings');

		return storedConnections ?? [];
	}

	// function testSave(){


	// 	storageManager.setValue('dclgentest', 'aaa');

	// 	const val = storageManager.getValue('dclgentest');

	// 	console.log(val);
	// }

	function saveConnectionStrings(connections: Config[]) {
		const storageManager = new LocalStorageService(context.globalState);
		storageManager.setValue<Config[]>('dclgen.sqlserver.connectionStrings', connections);
	}
}



// This method is called when your extension is deactivated
export function deactivate() { }
