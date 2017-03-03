#############################################
#   RelgramTuple
#   Classes to hold RelgramTuple data and 
#   read in RelgramTuples
#
# Relgram Strings Come in the following form:
# arg1 | arg1_normalized | arg1_types | rel | rel_normalized | arg2 | arg2_normalized | arg2_types
# 
#############################################



class RelgramTuple:
    'Hold the data for a single relgram'

    def __init__(self, arg1, arg1_type, rel, arg2, arg2_type, generalized = False):
        """
        String representations of arguments, argument types, and the normalized relation
        """
        self.arg1 = arg1.strip().lower()
        self.arg1_type = arg1_type.strip().lower()
        self.arg2 = arg2.strip().lower()
        self.arg2_type = arg2_type.strip().lower()
        self.relation = rel.strip().lower()
        self.generalized = generalized

    def getString(self):
        """
        Return a string represtation of the tuple
        """
        return "{}|{}|{}".format(self.arg1, self.relation, self.arg2)

    def getTupleString(self):
        """
        Return a string represtation of the tuple
        """
        return "({}, {}, {})".format(self.arg1, self.relation, self.arg2)

    def getGeneralizedTuples(self):
        """
        Return the set of generalized tuples
        These are tuples with one or two of the literal arguments replaced with its type 
        Return None if the argument doesnt have a type that allows a generalized tuple
        """
        if self.arg1_type and not self.arg1_type.isspace():
            arg1_replaced = RelgramTuple("type:" + self.arg1_type.split(':')[0], self.arg1_type, self.relation, self.arg2, self.arg2_type, True)
        else:
            arg1_replaced = None
        if self.arg2_type and not self.arg2_type.isspace():
            arg2_replaced = RelgramTuple(self.arg1, self.arg1_type, self.relation, "type:" + self.arg2_type.split(':')[0], self.arg2_type, True)
        else:
            arg2_replaced = None
        if arg1_replaced and arg2_replaced:
            both_replaced = RelgramTuple("type:" + self.arg1_type.split(':')[0], self.arg1_type, self.relation, "type:" + self.arg2_type.split(':')[0], self.arg2_type, True)
        else:
            both_replaced = None
        gen_tups = [arg1_replaced, arg2_replaced, both_replaced]
        return [x for x in gen_tups if x] #get rid of anything thats None

    def __str__(self):
        return self.getString()
        

class RelgramReader:
    'Produce RelgramTuples from text file'

    def __init__(self, sep_char = "|"):
        """
        sep_char indicates the charecter that seperates the arguments in the
        string input of a relational tuple
        """
        self.sep_char = sep_char

    def fromString(self, str_relgram):
        """
        Produce a new RelgramTuple from a string 
        """
    
        if str_relgram and not str_relgram.isspace():
            split_line = str_relgram.split(self.sep_char)
            if len(split_line) != 8:
                print("RelgramReader: Wrong number of arguments in string - {}".format(str_relgram))
                return None
            else:
                A1 = split_line[1]
                A1Types = split_line[2]
                A1Type = A1Types.split(',')[0] #take the first type listed

                rel = split_line[4]
                
                A2 = split_line[6]
                A2Types = split_line[7]
                A2Type = A2Types.split(',')[0] #take the first type listed
                return RelgramTuple(A1, A1Type, rel, A2, A2Types)
        else:
            print("RelgramReader: Empty String Given")
            return ""

    def fromFile(self, filename):
        """
        Return a list of RelgramTuples read from a file
        """
        relgrams_list = []
        with open(filename, 'r') as infile:
            for line in infile:
                relgram = self.fromString(line)
                if relgram is None:
                    print("RelgramReader: Malformed string")
                    return None
                if relgram != "":
                    relgrams_list.append(relgram)

            return relgrams_list


                 

