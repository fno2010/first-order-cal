#!/usr/bin/env python3
import ply.lex as lex
import ply.yacc as yacc
import re
import sys
if sys.version[0] == '3':
    raw_input = input

symbol = {
    'and' : 'And',
    'or' : 'Or',
    'not' : 'Not',
    'implic' : '=>',
    'equal' : '<=>',
    'exist' : 'Exist',
    'any' : 'Any'
    }

def display(result='', op=None):
    """
    Utility method for display
    """
    if op:
        sys.stdout.write("%s: %s\n\n" % (op, result))
    else:
        sys.stdout.write("%s\n" % result)

class LogicParser:
    def __init__(self):
        self.tree = {}
        #=============================================================
        # Part of Lexer
        #=============================================================
        reserved = {
            'Exist' : 'EXIST',
            'Any' : 'ANY',
            'And' : 'AND',
            'Or' : 'OR',
            'Not' : 'NOT'
        }

        tokens = [
            'VAR',
            'PRED',
            'IMPLIC',
            'EQUAL',
            ] + list(reserved.values())

        def t_VAR(t):
            r'[a-z][a-zA-Z0-9]*'
            t.type = reserved.get(t.value, 'VAR')
            return t

        def t_PRED(t):
            r'[A-Z][a-zA-Z0-9]*'
            t.type = reserved.get(t.value, 'PRED')
            return t

        t_AND = r'And|&'
        t_OR = r'Or|\|'
        t_NOT = r'Not|~'
        t_IMPLIC = r'=>'
        t_EQUAL = r'<=>'

        literals = ['[', ']', ',', '(', ')']

        t_ignore = ' \t'

        def t_error(t):
            message = t.lexpos
            s = "Lexical Error"
            raise lex.LexError(message, s)
        #   t.lexer.skip(1)

        self.Lexer = lex.lex()

        #=============================================================
        # Part of Parser
        #=============================================================

        # Exist Statement
        def p_exp_exist(p):
            "exp : EXIST '[' VAR ',' exp ']'"
            p[0] = {'name' : 'exist', 'var' : p[3], 'exp' : p[5]}

        # Any Statement
        def p_exp_any(p):
            "exp : ANY '[' VAR ',' exp ']'"
            p[0] = {'name' : 'any', 'var' : p[3], 'exp' : p[5]}

        # Not Statement
        def p_exp_not(p):
            "exp : NOT exp"
            p[0] = {'name' : 'not', 'exp' : p[2]}

        # And Statement
        def p_exp_and(p):
            "exp : exp AND exp"
            p[0] = {'name' : 'and', 'exp1' : p[1], 'exp2' : p[3]}

        # Or Statement
        def p_exp_or(p):
            "exp : exp OR exp"
            p[0] = {'name' : 'or', 'exp1' : p[1], 'exp2' : p[3]}

        # Implication Statement
        def p_exp_implic(p):
            "exp : exp IMPLIC exp"
            p[0] = {'name' : 'implic', 'exp1' : p[1], 'exp2' : p[3]}

        # Equivalent Statement
        def p_exp_equal(p):
            "exp : exp EQUAL exp"
            p[0] = {'name' : 'equal', 'exp1' : p[1], 'exp2' : p[3]}

        # Parentheses Statement
        def p_exp_paren(p):
            "exp : '(' exp ')'"
            p[0] = p[2]

        # Predicate Function
        def p_exp_pred(p):
            "exp : PRED '[' var_list ']'"
            p[0] = {'name' : 'pred', 'pred' : p[1], 'arg' : p[3]}

        # First Argument of Variable List
        def p_var_list_begin(p):
            "var_list : VAR"
            p[0] = [p[1]]

        # Next Argument of Variable List
        def p_var_list_next(p):
            "var_list : var_list ',' VAR"
            p[0] = p[1] + [p[3]]

        # Error Handle
        def p_error(p):
            message = p.lexpos
            raise yacc.YaccError(message)

        self.Parser = yacc.yacc()

    # Eliminate Implication
    def eli_implic(self, exp):
        """
        Eliminate All Implication Statement and Equivalent Statement
        in the Tree 'exp'.

        Return None
        """
        name = exp['name']
        if 'implic' == name:
            exp['name'] = 'or'
            exp['exp1'] = {'name' : 'not', 'exp' : exp['exp1'].copy()}
            exp['exp2'] = exp['exp2']
        elif 'equal' == name:
            exp['name'] = 'and'
            exp1 = exp['exp1']
            exp2 = exp['exp2']
            exp['exp1'] = {
                'name' : 'implic',
                'exp1' : exp1.copy(),
                'exp2' : exp2.copy()
                }
            exp['exp2'] = {
                'name' : 'implic',
                'exp1' : exp2.copy(),
                'exp2' : exp1.copy()
                }
        # Recursive
        name = exp['name']
        if 'exist' == name or 'any' == name or 'not' == name:
            self.eli_implic(exp['exp'])
        elif 'and' == name or 'or' == name:
            self.eli_implic(exp['exp1'])
            self.eli_implic(exp['exp2'])

    # Move Not Inwards
    def move_not_in(self, exp):
        """
        Modify the Tree 'exp' to move Not inwards.

        Return None
        """
        name = exp['name']
        if 'not' == name:
            subexp = exp['exp']
            if 'exist' == subexp['name']:
                field = subexp['exp']
                exp['name'] = 'any'
                exp['var'] = subexp['var']
                exp['exp'] = {'name' : 'not', 'exp' : field.copy()}
            elif 'any' == subexp['name']:
                field = subexp['exp']
                exp['name'] = 'exist'
                exp['var'] = subexp['var']
                exp['exp'] = {'name' : 'not', 'exp' : field.copy()}
            elif 'and' == subexp['name']:
                exp1 = subexp['exp1']
                exp2 = subexp['exp2']
                exp['name'] = 'or'
                exp['exp1'] = {'name' : 'not', 'exp' : exp1.copy()}
                exp['exp2'] = {'name' : 'not', 'exp' : exp2.copy()}
                del exp['exp']
            elif 'or' == subexp['name']:
                exp1 = subexp['exp1']
                exp2 = subexp['exp2']
                exp['name'] = 'and'
                exp['exp1'] = {'name' : 'not', 'exp' : exp1.copy()}
                exp['exp2'] = {'name' : 'not', 'exp' : exp2.copy()}
                del exp['exp']
            elif 'not' == subexp['name']:
                subsubexp = subexp['exp']
                del exp['name']
                del exp['exp']
                for key in subsubexp.keys():
                    exp[key] = subsubexp[key]
        # Recursive
        name = exp['name']
        if 'not' == name or 'exist' == name or 'any' == name:
            self.move_not_in(exp['exp'])
        elif 'and' == name or 'or' == name:
            self.move_not_in(exp['exp1'])
            self.move_not_in(exp['exp2'])

    # Standardize Variables
    def next_label(self, var, ever):
        """
        Get Next LabelNo of var From ever.

        var - input variable name
        ever - a set of the variables used ever

        Return the next var label considered by ever
        """
        if var in ever:
            p = re.compile('\d*$')
            m = p.search(var)
            labelno = m.group()
            start = m.start()
            p = re.compile(var[:start]+"\d+'")
            latest = [x[start:-1] for x in p.findall(str(ever))]
            latest = max(latest) if latest else ""
            labelno = max(labelno, latest)
            labelno = str(int(labelno)+1) if labelno else "1"
            return var[:start] + labelno
        else:
            return var

    def standardize_var(self, exp, ever):
        """
        Standardize the Variables of Tree 'exp'.

        exp - the Tree
        ever - a set of the variables used ever

        Return a set of the variables in the Tree 'exp'
        """
        name = exp['name']
        field = []
        if 'and' == name or 'or' == name:
            field = self.standardize_var(exp['exp1'], ever)
            field += self.standardize_var(exp['exp2'], ever + field)
        elif 'not' == name:
            field = self.standardize_var(exp['exp'], ever)
        elif 'exist' == name or 'any' == name:
            exp['var'] = self.next_label(exp['var'], ever)
            field = [exp['var']]
            field += self.standardize_var(exp['exp'], ever)
        elif 'pred' == name:
            arglist = exp['arg']
            exp['arg'] = [self.next_label(x, ever) for x in arglist]
        return field

    # skolemize
    def skolem(self, exp, rule, field):
        """
        Drop Exist Quantifiers by skolemize.

        exp - the Tree
        rule - the Rule of Variables Mapping
        field - a list of variables

        Return None
        """
        name = exp['name']
        if 'any' == name:
            self.skolem(exp['exp'], rule, field + [exp['var']])
        elif 'exist' == name:
            var = exp['var']
            newrule = rule.copy()
            newrule[var] = '_' + var.upper()
            if field:
                newrule[var] += '(' + field[0]
                for x in field[1:]:
                    newrule[var] += ', ' + x
                newrule[var] += ')'
            subexp = exp['exp'].copy()
            exp.clear()
            for key in subexp.keys():
                exp[key] = subexp[key]
            self.skolem(exp, newrule, field)
        elif name in ['and', 'or']:
            self.skolem(exp['exp1'], rule, field)
            self.skolem(exp['exp2'], rule, field)
        elif 'not' == name:
            self.skolem(exp['exp'], rule, field)
        elif 'pred' == name:
            arglist = exp['arg']
            exp['arg'] = [rule[x] if x in rule else x for x in arglist]

    # Drop Universal Quantifiers
    def drop_univ_quan(self, exp):
        """
        Drop Universal Quantifiers Straightly:

        Return None
        """
        name = exp['name']
        if name in ['and', 'or']:
            self.drop_univ_quan(exp['exp1'])
            self.drop_univ_quan(exp['exp2'])
        elif 'not' == name:
            self.drop_univ_quan(exp['exp'])
        elif 'any' == name:
            field = exp['exp'].copy()
            exp.clear()
            for key in field.keys():
                exp[key] = field[key]
            self.drop_univ_quan(exp)

    # Convert To CNF
    def convert_to_cnf(self, exp):
        """
        Convert To CNF by Distribute OR Over AND

        Return None
        """
        name = exp['name']
        if 'or' == name:
            self.convert_to_cnf(exp['exp1'])
            self.convert_to_cnf(exp['exp2'])
        elif 'and' == name:
            exp1 = exp['exp1']
            exp2 = exp['exp2']
            if 'or' == exp1['name']:
                exp['name'] = 'or'
                exp['exp1'] = {
                    'name' : 'and',
                    'exp1' : exp1['exp1'],
                    'exp2' : exp2.copy()
                    }
                exp['exp2'] = {
                    'name' : 'and',
                    'exp1' : exp1['exp2'],
                    'exp2' : exp2.copy()
                    }
            elif 'or' == exp2['name']:
                exp['name'] = 'or'
                exp['exp1'] = {
                    'name' : 'and',
                    'exp1' : exp1.copy(),
                    'exp2' : exp2['exp1']
                    }
                exp['exp2'] = {
                    'name' : 'and',
                    'exp1' : exp1.copy(),
                    'exp2' : exp2['exp2']
                    }
            self.convert_to_cnf(exp['exp1'])
            self.convert_to_cnf(exp['exp2'])

    # Show the Expression from the Tree
    def show_exp(self, exp):
        name = exp['name']
        if name in ['implic', 'equal']:
            exp1 = exp['exp1']
            if exp1['name'] in ['and', 'or', 'implic', 'equal']:
                exp1show = '(' + self.show_exp(exp1) + ')'
            else:
                exp1show = self.show_exp(exp1)
            exp2 = exp['exp2']
            if exp2['name'] in ['and', 'or', 'implic', 'equal']:
                exp2show = '(' + self.show_exp(exp2) + ')'
            else:
                exp2show = self.show_exp(exp2)
            return exp1show + ' ' + symbol[name] + ' ' + exp2show
        elif name in ['and', 'or']:
            exp1 = exp['exp1']
            L = {'and', 'or', 'implic', 'equal'} - {name}
            if exp1['name'] in L:
                exp1show = '(' + self.show_exp(exp1) + ')'
            else:
                exp1show = self.show_exp(exp1)
            exp2 = exp['exp2']
            if exp2['name'] in L:
                exp2show = '(' + self.show_exp(exp2) + ')'
            else:
                exp2show = self.show_exp(exp2)
            return exp1show + ' ' + symbol[name] + ' ' + exp2show
        elif name in ['exist', 'any']:
            var = exp['var']
            subexp = self.show_exp(exp['exp'])
            return symbol[name] + '[%s, %s]' % (var, subexp)
        elif 'not' == name:
            subexp = exp['exp']
            if subexp['name'] in ['and', 'or', 'implic', 'equal']:
                expshow = '(' + self.show_exp(subexp) + ')'
            else:
                expshow = self.show_exp(subexp)
            return 'Not ' + expshow
        elif 'pred' == name:
            expshow = exp['pred'] + '[' + exp['arg'][0]
            for x in exp['arg'][1:]:
                expshow += ', ' + x
            expshow += ']'
            return expshow
        return ''

    # Parse the First-Order Logic Statement
    def exec(self, statement):
        try:
            self.tree = self.Parser.parse(statement)
            display(self.show_exp(self.tree), 'Original')
            self.eli_implic(self.tree)
            display(self.show_exp(self.tree), 'eli_implic')
            self.move_not_in(self.tree)
            display(self.show_exp(self.tree), 'move_not_in')
            self.standardize_var(self.tree, [])
            display(self.show_exp(self.tree), 'standardize_var')
            self.skolem(self.tree, {}, [])
            display(self.show_exp(self.tree), 'skolemize')
            self.drop_univ_quan(self.tree)
            display(self.show_exp(self.tree), 'drop_univ_quan')
            self.convert_to_cnf(self.tree)
            display(self.show_exp(self.tree), 'convert_to_cnf')
        except lex.LexError as e:
            display(statement)
            display(' ' * e.args[0] + '^')
            display(e.text)
            display()
        except yacc.YaccError as e:
            display(statement)
            display(' ' * e.args[0] + '^')
            display('Syntax Error')
            display()


p = LogicParser()
example = 'Any[x, Any[y, (Person[x] & Person[y] & \
Exist[z, Parent[x,z] & Parent[y,z]])=>(Couple[x,y] & Couple[y,x])]]'

usage = """
help for First-Order Logic Parser,
You can input a First-Order Logic Statement,
And it will be converted to CNF Fromat.

Command:
help, h     get help
quit, q     quit the command
example, e  show an example

Format:
exist:        Exist[var, exp]
any:          Any[var, exp]
and:          exp1 And exp2 / exp1 & exp2
or:           exp1 Or exp2 / exp1 | exp2
not:          Not exp / ~ exp
implication:  exp1 => exp2
equivalient:  exp1 <=> exp2
"""

def show_help():
    display(usage)

if '__main__' == __name__:
    show_help()

    while True:
        command = raw_input('> ')
        if command in ['quit', 'q']:
            break
        elif command in ['help', 'h']:
            show_help()
        elif command in ['example', 'e']:
            display('> ' + example)
            p.exec(example)
        else:
            p.exec(command)
