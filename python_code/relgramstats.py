################################################
#   RelgramStats 
#   Methods for calculating varioius statistics
#   given relgram count data (a RelgramCounter)
#   Use for calculating conditional probabilities
#   and symettric conditional probabilities
################################################
from relgramcounter import RelgramCounter, RelgramCounterReader
from relgramtuple import RelgramTuple


def windowConditionalProb(counter, t2, t1, k, delta=0.05):
    """Calculate P_k(t2 | t1), a delta smoothed estimate of the conditional probability
        of the two tuples t1, t2, within a window of k.
        params:
            RelgramCounter counter
            String t1, t2
    """
    rel_map = counter.relgram_map
    if k in counter.windows:
        window_index = counter.windows.index(k)
    else:
        print("Relgram Statistics for k = {} not computed!".format(k))
        return None

    if t2 in rel_map[t1]:
        t1_t2_count = rel_map[t1][t2][window_index] #number of times t2_appears after t1 within k
    else:
        t1_t2_count = 0

    Z = 0 #nomalization term
    t1_stats = rel_map[t1]
    for t3 in t1_stats:  #get counts of all tuples that appear after t1 within window of k
        Z += t1_stats[t3][window_index]

    V = len(rel_map.keys()) #number of unique tuples in corpus

    prob = (t1_t2_count + delta) / (Z + delta*V)
    return prob

def conditionalProb(counter, t2, t1, alpha=0.5, delta=0.05):
    """
    Calculate the conditional probability P(t2 | t1) across all window sizes used,
    alpha is the weight decay constant (further windows give less weight in 
    the probability calculation)
    """
    score = 0
    Z = 0
    for window in counter.windows:
        Z += (alpha ** window)
        score += (alpha ** window) * windowConditionalProb(counter, t2, t1, window, delta)

    prob = score / Z
    return prob

def SCP(counter, t1, t2, alpha=0.5, delta=0.05):
    """
    Calculate the symetric conditional probability between t1 and t2
    SCP(t1,t2) = P(t2|t1)P(t1|t2)
    """
    prob1 = conditionalProb(counter, t1, t2, alpha, delta)
    prob2 = conditionalProb(counter, t2, t1, alpha, delta)
    return prob1*prob2


def getTopK(counter, tup, k=25):
    """
    Return a list of k tuples (string, doulbe) containing the tuples
    (string form) closest to 
    tup (in string form) tup, as well as the SCP between them
    """
    adj_list = [] #list of tuples that co occur with tup at least once
    for t in counter.relgram_map[tup]:
        adj_list.append(t) #add all that appear after tup


    for i in counter.relgram_map: #find any that appear before tup
        for j in counter.relgram_map[i]:
            if j == tup and i not in adj_list: 
                adj_list.append(i)

    scores = [(t, SCP(counter, t, tup)) for t in adj_list] 
    return sorted(scores, key=lambda x: x[1], reverse=True)

    
def printTop(counter, tup, filename, twohop=False, k=25):
    sep = "_NSEP_"
    top = getTopK(counter, tup)
    print(top)
    with open(filename, 'w') as outfile:
        tab_tup = tup.replace('|', '\t')

#        tab_tup_wargs = tab_tup + '\t0\t\tNONE\t' + '\t'.join(counter.relgram_args[tup])
        tab_tup_wargs = tab_tup + '\t0\t\tNONE\t' + counter.getInstanceString(tup)
        outfile.write("seed" + sep + tab_tup + "\n")

        for i in top:
            relgram = i[0]
            scp = i[1]
            outfile.write("onehop{}{}{}{}{}{}\n".format(sep, tab_tup_wargs, sep, relgram.replace('|', '\t') + '\t0\t\tNONE\t' + counter.getInstanceString(relgram), sep, scp))
        if twohop:
            for i in top[:k]:
                tup_twohop = i[0]
                #if tup_twohop in counter.relgram_map: #make sure this tuple has outgoing nodes
                top_twohop = getTopK(counter, tup_twohop)
                tab_tup_twohop = tup_twohop.replace('|', '\t')
                tab_tup_twohop_wargs = tab_tup_twohop + '\t0\t\tNONE\t' + counter.getInstanceString(tup_twohop)
                for i in top_twohop:
                    relgram = i[0]
                    scp = i[1]
                    if relgram != tup:
                        outfile.write("twohop{}{}{}{}{}{}\n".format(sep, tab_tup_twohop_wargs, sep, relgram.replace('|', '\t') + '\t0\t\tNONE\t' + counter.getInstanceString(relgram), sep, scp))


            

def printTopNoArgs(counter, tup, filename, twohop=False, k=25):
    sep = "_NSEP_"
    top = getTopK(counter, tup)
    print(top)
    with open(filename, 'w') as outfile:
        tab_tup = tup.replace('|', '\t')

        outfile.write("seed" + sep + tab_tup + "\n")

        for i in top:
            relgram = i[0]
            scp = i[1]
            outfile.write("onehop{}{}{}{}{}{}\n".format(sep, tab_tup, sep, relgram.replace('|', '\t'), sep, scp))
        if twohop:
            for i in top[:k]:
                tup_twohop = i[0]
                #if tup_twohop in counter.relgram_map: #make sure this tuple has outgoing nodes
                top_twohop = getTopK(counter, tup_twohop)
                tab_tup_twohop = tup_twohop.replace('|', '\t')
                for i in top_twohop:
                    relgram = i[0]
                    scp = i[1]
                    if relgram != tup:
                        outfile.write("twohop{}{}{}{}{}{}\n".format(sep, tab_tup_twohop, sep, relgram.replace('|', '\t'), sep, scp))
   
        
    
