import { CstNode, GrammarAST, DefaultValueConverter, ValueType } from 'langium';

export class Lam4ValueConverter extends DefaultValueConverter {

    protected override runConverter(rule: GrammarAST.AbstractRule, input: string, cstNode: CstNode): ValueType {
        if (rule.name.startsWith('SINGLELINE_METADATA_ANN')) {
            // Remove "--" and any leading spaces
            return input.substring(2).trimStart();
        } else if (rule.name.startsWith('BACK_TICKED')) {
            // Remove the first and last character (the backticks)
            return input.substring(1, input.length - 1);
        } else {
            return super.runConverter(rule, input, cstNode);
        }
    }
}