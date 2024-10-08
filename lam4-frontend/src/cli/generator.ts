import { JSONString } from '../utils.js';
import type { Program } from '../language/generated/ast.js';
// import { expandToNode, joinToNode, toString } from 'langium/generate';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { extractDestinationAndName } from './cli-util.js';
import { createLam4Services } from '../language/lam4-module.js';
import { NodeFileSystem } from 'langium/node';
import type {JsonSerializeOptions} from 'langium';

// TODO: Set up the auto formatting that adds auto indents for properties
const DEFAULT_SERIALIZATION_CONFIG: JsonSerializeOptions = 
    {space: 4,
     refText: true,
     sourceText: true,
     textRegions: true,
     comments: false}

export function serializeProgramToJson(program: Program, serializationConfig: JsonSerializeOptions = DEFAULT_SERIALIZATION_CONFIG): JSONString {
    const services = createLam4Services(NodeFileSystem).lam4Services;
    
    const astJson = services.serializer.JsonSerializer.serialize(program, serializationConfig);    
    return astJson as JSONString;
}

export function writeToDisk(data: string, filePath: string, nameModifier: string = "", destination: string | undefined, fileExt: string = ".json"): string {
    const filepath = extractDestinationAndName(filePath, destination);
    const finalFileName = filepath.name + nameModifier;
    const outFilePath = `${path.join(filepath.destination, finalFileName)}${fileExt}`;

    if (!fs.existsSync(filepath.destination)) {
        fs.mkdirSync(filepath.destination, { recursive: true });
    }
    fs.writeFileSync(outFilePath, data);
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
