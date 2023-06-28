// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';


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
		
		const connectionString = await vscode.window.showInputBox({
			prompt: 'Enter the connection string',
			placeHolder: 'Server=<server>;Database=<database>;User Id=<user>;Password=<password>;',
		  });
		
		  if (connectionString) {
			await generateTablesFile(connectionString);
		  }

		


	});

	context.subscriptions.push(disposable);

	async function generateTablesFile(conn: string){
		const sql = require('mssql');

		const config = {
			user: 'sa',
			password: 'karj1sf',
			server: 'localhost',
			database: 'Birding',
			// port: 1433,
			options: {
				enableArithAbort: true,
				encrypt: true, // for azure
				trustServerCertificate: true // change to true for local dev / self-signed certs
			},
		};

		// vscode.window.showInformationMessage('before pool!');
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


		let dclgen = " ".repeat(9) + "01 DCLGEN_" + table.toUpperCase() + ".\n";

		let dclgenNulls = " ".repeat(9) + "01 DCLGEN_" + table.toUpperCase() + "_NULLS.\n";

		tableFields.forEach((field: any) => {
			dclgen += " ".repeat(12) + "03 " + field.COLUMN_NAME.toUpperCase() + " ";
			switch (field.DATA_TYPE) {
				case 'int':
					dclgen += "PIC 9(9)";
					break;
				case 'nvarchar':
				case 'varchar':
					dclgen += "PIC X(" + field.CHARACTER_MAXIMUM_LENGTH.toString() + ")";
					break;
				case 'datetime':
				case 'datetime2':
					dclgen += "PIC X(28)";
					break;
				case 'float':
					dclgen += "PIC S9(13).99";
			}

			dclgen += '.\n';

			if (field.IS_NULLABLE === 'YES') {
				anyFieldNotNull = true;
				dclgenNulls += " ".repeat(12) + "03 " + field.COLUMN_NAME.toUpperCase() + "NULL PIC 9(2) VALUE 0.\n";
			}

		});

		if (anyFieldNotNull) { 
			dclgen += '\n\n\n' + dclgenNulls; 
		}

		return dclgen;
	}
}



// This method is called when your extension is deactivated
export function deactivate() { }
