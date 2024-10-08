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

export async function getProgramAst(fileName: string) {
    const services = createLam4Services(NodeFileSystem).lam4Services;
    const program = await extractAstNode<Program>(fileName, services);
    return program;
} 

export function consoleLogAST(ast: string) {
    const prettiedStr = chalk.cyan("<ast_from_frontend>" + ast + "</ast_from_frontend>");
    console.log(prettiedStr);
}

export async function toMinimalAst(fileName: string, opts: GenerateOptions) {
    const noMetadataSerializationConfig = {
        space: 4,
        refText: true,
        sourceText: false,
        textRegions: false,
        comments: false
    }

    const program = await getProgramAst(fileName);
    const noMetadataAstString = serializeProgramToJson(program, noMetadataSerializationConfig) as string;
    const astJsonOutPath = writeToDisk(noMetadataAstString, fileName, 
        "", opts.destination);

    consoleLogAST(noMetadataAstString);
    console.log(chalk.green(`AST without source metadata serialized to JSON at ${astJsonOutPath}`));

}

export const toAstAction = async (fileName: string, opts: GenerateOptions): Promise<void> => {
    const program = await getProgramAst(fileName);

    // const generatedFilePath = generateJavaScript(program, fileName, opts.destination);
    // console.log(chalk.green(`JavaScript code generated successfully: ${generatedFilePath}`));

    const astString = serializeProgramToJson(program) as string;
    consoleLogAST(astString);

    // for quick debugging
    // const parsedJson = JSON.parse(astString);
    // console.log(chalk.cyan(JSON.stringify(parsedJson, null, 2)));


    const astJsonOutPath = writeToDisk(astString, fileName, "_with_source_metadata", opts.destination);
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
        .command('toAstWithSrcMetadata')
        .argument('<file>', `source file (possible file extensions: ${fileExtensions})`)
        .option('-d, --destination <dir>', 'destination directory')
        .description('print and serialize concrete syntax (with source text metadata) to JSON')
        .action(toAstAction);

    program
        .command('toMinimalAst')
        .argument('<file>', `source file (possible file extensions: ${fileExtensions})`)
        .option('-d, --destination <dir>', 'destination directory')
        .description('print and serialize more minimal version of concrete syntax -- i.e., *without metadata*')
        .action(toMinimalAst);

    program.parse(process.argv);
}
