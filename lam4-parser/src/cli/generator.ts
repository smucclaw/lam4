import { JSONString } from '../utils.js';
import type { Program } from '../language/generated/ast.js';
// import { expandToNode, joinToNode, toString } from 'langium/generate';
import chalk from 'chalk';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { extractDestinationAndName } from './cli-util.js';
import { createLam4Services } from '../language/lam4-module.js';
import { NodeFileSystem } from 'langium/node';

export function generateAST(program: Program): JSONString {
    const services = createLam4Services(NodeFileSystem).lam4Services;
    
    const astJson = services.serializer.JsonSerializer.serialize(program);

    // for quick debugging
    const parsedJson = JSON.parse(astJson);
    console.log(chalk.cyan(JSON.stringify(parsedJson, null, 2)));
    
    return astJson as JSONString;
}

export function generateAndSaveAST(program: Program, filePath: string, destination: string | undefined): string {
    const data = extractDestinationAndName(filePath, destination);
    const outFilePath = `${path.join(data.destination, data.name)}.json`;

    const astJson = generateAST(program);

    if (!fs.existsSync(data.destination)) {
        fs.mkdirSync(data.destination, { recursive: true });
    }
    fs.writeFileSync(outFilePath, astJson);
    return outFilePath;
}

export function generateJavaScript(program: Program, filePath: string, destination: string | undefined): string {
    const data = extractDestinationAndName(filePath, destination);
    const generatedFilePath = `${path.join(data.destination, data.name)}.js`;

    // const fileNode = expandToNode`
    //     "use strict";

    //     ${joinToNode(model.greetings, greeting => `console.log('Hello, ${greeting.person.ref?.name}!');`, { appendNewLineIfNotEmpty: true })}
    // `.appendNewLineIfNotEmpty();

    if (!fs.existsSync(data.destination)) {
        fs.mkdirSync(data.destination, { recursive: true });
    }
    // fs.writeFileSync(generatedFilePath, toString(fileNode));
    return generatedFilePath;
}
