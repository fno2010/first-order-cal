import ply.lex as lex
import ply.yacc as yacc
import re

symbol = {
    'and' : 'And',
    'or' : 'Or',
    'not' : 'Not',
    'implic' : '=>',
    'equal' : '<=>',
    'exist' : 'Exist',
    'any' : 'Any'
    }

class logicParser:
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
    def EliImplic(self, exp):
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
            self.EliImplic(exp['exp'])
        elif 'and' == name or 'or' == name:
            self.EliImplic(exp['exp1'])
            self.EliImplic(exp['exp2'])

    # Move Not Inwards
    def MovNotIn(self, exp):
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
            self.MovNotIn(exp['exp'])
        elif 'and' == name or 'or' == name:
            self.MovNotIn(exp['exp1'])
            self.MovNotIn(exp['exp2'])

    # Standardize Variables
    def NextLabel(self, var, ever):
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

    def StandVar(self, exp, ever):
        """
        Standardize the Variables of Tree 'exp'.
        
        exp - the Tree
        ever - a set of the variables used ever
        
        Return a set of the variables in the Tree 'exp'
        """
        name = exp['name']
        field = []
        if 'and' == name or 'or' == name:
            field = self.StandVar(exp['exp1'], ever)
            field += self.StandVar(exp['exp2'], ever + field)
        elif 'not' == name:
            field = self.StandVar(exp['exp'], ever)
        elif 'exist' == name or 'any' == name:
            exp['var'] = self.NextLabel(exp['var'], ever)
            field = [exp['var']]
            field += self.StandVar(exp['exp'], ever)
        elif 'pred' == name:
            arglist = exp['arg']
            exp['arg'] = [self.NextLabel(x, ever) for x in arglist]
        return field

    # Skolemize
    def Skolem(self, exp, rule, field):
        """
        Drop Exist Quantifiers by Skolemize.

        exp - the Tree
        rule - the Rule of Variables Mapping
        field - a list of variables

        Return None
        """
        name = exp['name']
        if 'any' == name:
            self.Skolem(exp['exp'], rule, field + [exp['var']])
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
            self.Skolem(exp, newrule, field)
        elif name in ['and', 'or']:
            self.Skolem(exp['exp1'], rule, field)
            self.Skolem(exp['exp2'], rule, field)
        elif 'not' == name:
            self.Skolem(exp['exp'], rule, field)
        elif 'pred' == name:
            arglist = exp['arg']
            exp['arg'] = [rule[x] if rule.has_key(x) else x
            for x in arglist]

    # Drop Universal Quantifiers
    def DropUnivQuan(self, exp):
        """
        Drop Universal Quantifiers Straightly:

        Return None
        """
        name = exp['name']
        if name in ['and', 'or']:
            self.DropUnivQuan(exp['exp1'])
            self.DropUnivQuan(exp['exp2'])
        elif 'not' == name:
            self.DropUnivQuan(exp['exp'])
        elif 'any' == name:
            field = exp['exp'].copy()
            exp.clear()
            for key in field.keys():
                exp[key] = field[key]
            self.DropUnivQuan(exp)

    # Convert To CNF
    def ConvertToCNF(self, exp):
        """
        Convert To CNF by Distribute OR Over AND

        Return None
        """
        name = exp['name']
        if 'or' == name:
            self.ConvertToCNF(exp['exp1'])
            self.ConvertToCNF(exp['exp2'])
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
            self.ConvertToCNF(exp['exp1'])
            self.ConvertToCNF(exp['exp2'])
    
    # Show the Expression from the Tree
    def ShowExp(self, exp):
        name = exp['name']
        if name in ['implic', 'equal']:
            exp1 = exp['exp1']
            if exp1['name'] in ['and', 'or', 'implic', 'equal']:
                exp1show = '(' + self.ShowExp(exp1) + ')'
            else:
                exp1show = self.ShowExp(exp1)
            exp2 = exp['exp2']
            if exp2['name'] in ['and', 'or', 'implic', 'equal']:
                exp2show = '(' + self.ShowExp(exp2) + ')'
            else:
                exp2show = self.ShowExp(exp2)
            return exp1show + ' ' + symbol[name] + ' ' + exp2show
        elif name in ['and', 'or']:
            exp1 = exp['exp1']
            L = {'and', 'or', 'implic', 'equal'} - {name}
            if exp1['name'] in L:
                exp1show = '(' + self.ShowExp(exp1) + ')'
            else:
                exp1show = self.ShowExp(exp1)
            exp2 = exp['exp2']
            if exp2['name'] in L:
                exp2show = '(' + self.ShowExp(exp2) + ')'
            else:
                exp2show = self.ShowExp(exp2)
            return exp1show + ' ' + symbol[name] + ' ' + exp2show
        elif name in ['exist', 'any']:
            var = exp['var']
            subexp = self.ShowExp(exp['exp'])
            return symbol[name] + '[%s, %s]' % (var, subexp)
        elif 'not' == name:
            subexp = exp['exp']
            if subexp['name'] in ['and', 'or', 'implic', 'equal']:
                expshow = '(' + self.ShowExp(subexp) + ')'
            else:
                expshow = self.ShowExp(subexp)
            return 'Not ' + expshow
        elif 'pred' == name:
            expshow = exp['pred'] + '[' + exp['arg'][0]
            for x in exp['arg'][1:]:
                expshow += ', ' + x
            expshow += ']'
            return expshow
        return ''

    # Parse the First-Order Logic Statement
    def Exec(self, Statement):
        try:
            self.tree = self.Parser.parse(Statement)
            print 'Oringal: ', self.ShowExp(self.tree)
            print
            self.EliImplic(self.tree)
            print 'EliImplic: ', self.ShowExp(self.tree)
            print
            self.MovNotIn(self.tree)
            print 'MovNotIn: ', self.ShowExp(self.tree)
            print
            self.StandVar(self.tree, [])
            print 'StandVar: ', self.ShowExp(self.tree)
            print
            self.Skolem(self.tree, {}, [])
            print 'Skolemize: ', self.ShowExp(self.tree)
            print
            self.DropUnivQuan(self.tree)
            print 'DropUnivQuan: ', self.ShowExp(self.tree)
            print
            self.ConvertToCNF(self.tree)
            print 'ConvertToCNF: ', self.ShowExp(self.tree)
            print
        except lex.LexError as e:
            print Statement
            print ' ' * e.args[0] + '^'
            print e.text
            print
        except yacc.YaccError as e:
            print Statement
            print ' ' * e.args[0] + '^'
            print 'Syntax Error'
            print


p = logicParser()
example = 'Any[x, Any[y, (Person[x] & Person[y] & \
Exist[z, Parent[x,z] & Parent[y,z]])=>(Couple[x,y] & Couple[y,x])]]'

def ShowHelp():
    print 'help for First-Order Logic Parser,'
    print 'You can input a First-Order Logic Statement,'
    print 'And it will be converted to CNF Fromat.'
    print
    print 'Command:'
    print 'help, h     get help'
    print 'quit, q     quit the command'
    print 'example, e  show an example'
    print
    print 'Format:'
    print 'exist:        Exist[var, exp]'
    print 'any:          Any[var, exp]'
    print 'and:          exp1 And exp2 / exp1 & exp2'
    print 'or:           exp1 Or exp2 / exp1 | exp2'
    print 'not:          Not exp / ~ exp'
    print 'implication:  exp1 => exp2'
    print 'equivalient:  exp1 <=> exp2'
    print

ShowHelp()

while True:
    command = raw_input('> ')
    if command in ['quit', 'q']:
        break
    elif command in ['help', 'h']:
        ShowHelp()
    elif command in ['example', 'e']:
        print '> ' + example
        p.Exec(example)
    else:
        p.Exec(command)

        
