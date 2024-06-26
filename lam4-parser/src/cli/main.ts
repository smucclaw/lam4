import type { Program } from '../language/generated/ast.js';
import chalk from 'chalk';
import { Command } from 'commander';
import { Lam4LanguageMetaData } from '../language/generated/module.js';
import { createLam4Services } from '../language/lam4-module.js';
import { extractAstNode } from './cli-util.js';
import { 
    // generateJavaScript, 
    serializeProgramToJson,
    writeToDisk } from './generator.js';
import { NodeFileSystem } from 'langium/node';
import * as url from 'node:url';
import * as fs from 'node:fs/promises';
import * as path from 'node:path';
const __dirname = url.fileURLToPath(new URL('.', import.meta.url));

const packagePath = path.resolve(__dirname, '..', '..', 'package.json');
const packageContent = await fs.readFile(packagePath, 'utf-8');

export const toAstAction = async (fileName: string, opts: GenerateOptions): Promise<void> => {
    const services = createLam4Services(NodeFileSystem).lam4Services;
    const program = await extractAstNode<Program>(fileName, services);
    // const generatedFilePath = generateJavaScript(program, fileName, opts.destination);
    // console.log(chalk.green(`JavaScript code generated successfully: ${generatedFilePath}`));

    const astString = serializeProgramToJson(program) as string;
    const astJsonOutPath = writeToDisk(astString, fileName, opts.destination);
    console.log(chalk.green(`AST serialized to JSON at ${astJsonOutPath}`));
};

export type GenerateOptions = {
    destination?: string;
}

export default function(): void {
    const program = new Command();

    program.version(JSON.parse(packageContent).version);

    const fileExtensions = Lam4LanguageMetaData.fileExtensions.join(', ');
    program
        .command('toAst')
        .argument('<file>', `source file (possible file extensions: ${fileExtensions})`)
        .option('-d, --destination <dir>', 'destination directory of generating')
        .description('serialize AST to JSON')
        .action(toAstAction);

    program.parse(process.argv);
}
