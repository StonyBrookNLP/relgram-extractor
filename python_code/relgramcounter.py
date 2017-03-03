###############################
#   Relgram Counter
# Calculate Relgram statistics 
# for a set of windows
##################################

from relgramtuple import RelgramTuple, RelgramReader
import numpy as np
import sys
import os

class RelgramCounter:
    'Finds and stores relgram statistics for a set of given windows'
    #relgram_args mapsstring form of tuples to a 2-tuple of dicts, each dict mapping argument instance to argument count (either for first or second argument position)
    def __init__(self, windows, include_generalized = True, relgram_map = {}, relgram_args = {}):
        """
        k-tuple windows - Whiche windows to calulate relgram statistics for,
        window sizes SHOULD BE IN order from least to greatest 
        include_generalized is a flag on whether or not to include counts for generalized tuples as well
        """
        self.windows = windows
        self.relgram_map = relgram_map
        self.generalized = include_generalized
        self.relgram_args = relgram_args
        #relgram_map maps string form of tuples to another dictornary, the statistics_map. 
        #This tuple indicates the first tuple in the relgram
        #The statistics_map maps the set of other tuples (the second tuple in the relgram)
        #These map to a k numpy array, each containing the relgram statistic for the pair of 
        #tuples as indicated by windows

    def update(self, tuples):
        """
        Update the relgram statistics by a list of RelgramTuple objects (assumed to be part
        of the same document)
        """
        if self.generalized:
            self.updateGeneralized(tuples)
            return 

        for tup_index in range(len(tuples)):
            str_tup = tuples[tup_index].getString()
            if str_tup not in self.relgram_map:
                self.relgram_map[str_tup] = {} #create new entry in relgram map
            stat_map = self.relgram_map[str_tup]
            for window_index in range(len(self.windows)):
                if window_index != 0:
                    #start looking at tuples the come after the window that we previously looked (to reduce redundent computations)
                    #of course this is assumeing the windows are in order from least to greatest
                    #start_window is how many places ahead to start looking (stop at end_window)
                    #So for example start of 1 and end of 5 means look at the tuples 1 to 5 places ahead
                    start_window = self.windows[window_index - 1] + 1 
                    end_window = self.windows[window_index]
                else:
                    start_window = 1 
                    end_window = self.windows[window_index]

                for skips in range(start_window, end_window + 1):
                    other_index = tup_index + skips 
                    if other_index >= len(tuples): #if we have reached the end of the list just break
                        break

                    other_str_tup = tuples[other_index].getString()
                    if other_str_tup not in stat_map: #init to ktuple of zeros if not in
                        stat_map[other_str_tup] = np.zeros(len(self.windows), dtype=np.int)
                    #increament each statistic by one that is greater than the current window
                    #ie if something comes in a window of 2 than it counts also in a window within 3, etc
                    stats = stat_map[other_str_tup]
                    for stat_index in range(len(stats)): #increment counts
                        if self.windows[stat_index] >= start_window: # only increment for windows greater than the start window
                            stats[stat_index] += 1
                        
    def updateGeneralized(self, tuples):
        """
        Update the relgram statistics, with generalized tuples, by a list of RelgramTuple objects (assumed to be part
        of the same document)
        """
        for tup_index in range(len(tuples)):
            curr_tup = tuples[tup_index]
            gen_tuples = [curr_tup]
            gen_tuples.extend(curr_tup.getGeneralizedTuples())

            for tup in gen_tuples:
                str_tup = tup.getString()
                #print(str_tup)
                if str_tup not in self.relgram_map:
                    self.relgram_map[str_tup] = {} #create new entry in relgram map
                stat_map = self.relgram_map[str_tup]
                for window_index in range(len(self.windows)):
                    if window_index != 0:
                        #start looking at tuples the come after the window that we previously looked (to reduce redundent computations)
                        #of course this is assumeing the windows are in order from least to greatest
                        #start_window is how many places ahead to start looking (stop at end_window)
                        #So for example start of 1 and end of 5 means look at the tuples 1 to 5 places ahead
                        start_window = self.windows[window_index - 1] + 1 
                        end_window = self.windows[window_index]
                    else:
                        start_window = 1 
                        end_window = self.windows[window_index]

                    for skips in range(start_window, end_window + 1):
                        other_index = tup_index + skips 
                        if other_index >= len(tuples): #if we have reached the end of the list just break
                            break

                        other_tup = tuples[other_index]
                        other_gen_tuples = [other_tup]
                        other_gen_tuples.extend(other_tup.getGeneralizedTuples())

                        for other in other_gen_tuples:
                            other_str_tup = other.getString()
                            if other_str_tup not in stat_map: #init to ktuple of zeros if not in
                                stat_map[other_str_tup] = np.zeros(len(self.windows), dtype=np.int)
                            #increament each statistic by one that is greater than the current window
                            #ie if something comes in a window of 2 than it counts also in a window within 3, etc
                            stats = stat_map[other_str_tup]
                            for stat_index in range(len(stats)): #increment counts
                                if self.windows[stat_index] >= start_window: # only increment for windows greater than the start window
                                    stats[stat_index] += 1

                
    def getRelgramStat(self, T1, T2):
        """
        Get the relgram statistic (returned as a tuple giving statistic for each window size)
        for the relgram (T1, T2), tuples should be passed in string form
        """

        if T1 in self.relgram_map:
            if T2 in self.relgram_map[T1]:
                return self.relgram_map[T1][T2]

        return np.zeros(len(self.windows))
        
    def getInstanceString(self, rel):
        """
        Print out the argument instances in a format readable by relgraphs application
        """
        ESEP = "_ESEP_"
        CSEP = "_CSEP_"
        arg1_instances = ""
        arg2_instances = ""
        if rel not in self.relgram_args:
            return ""
        else:
            instances = self.relgram_args[rel]

        for ents in instances[0].items():
            arg1_instances += ESEP + CSEP.join((str(x) for x in ents)) 

        for ents in instances[1].items():
            arg2_instances += ESEP + CSEP.join((str(x) for x in ents)) 


        arg1_instances = arg1_instances[len(ESEP):]
        arg2_instances = arg2_instances[len(ESEP):]
        return arg1_instances + '\t' + arg2_instances
            

class RelgramCounterReader:
    'Class to create and output RelgramCounters'
        
    def fromDirectory(self, directory, windows, generalized = False):
        """
        Create RelgramCounter (with the passed in windows), data from tuple data in indicated directory,
        each file indicates a different article
        Returns the created RelgramCounter
        """
        reader = RelgramReader()
        counter = RelgramCounter(windows, generalized)
        for doc in os.listdir(directory):
            print("RelgramCounterReader: Processing file {}".format(doc))
            tups = reader.fromFile(directory + '/' + doc)
            if tups:
                counter.update(tups)
            else:
                print("Skipping file - {}".format(doc))
        return counter

    def printCounter(self, counter, filename, thresh = 25):
        """
        print the passed in RelgramCounter data to file
        """
        with open(filename, 'w') as outfile:
            outfile.write("Window sizes = {}\n".format(counter.windows))
            for T1 in counter.relgram_map:
                for T2 in counter.relgram_map[T1]:
                    if sum(counter.relgram_map[T1][T2]) > thresh:
                        string_stats = [str(x) for x in counter.relgram_map[T1][T2]]
                        outfile.write("{}\t{}\t{}\n".format(T1, T2, "\t".join(string_stats)))

    def convertToType(self, li):
        arg1 = li[0].split(':')
        arg2 = li[2].split(':')
        if len(arg1) > 1:
            li[0] = "type:" + arg1[0]
        if len(arg2) > 1:
            li[2] = "type:" + arg2[0]
        return li


    def updateArgCounts(self, relgram_args, rel, arg1, arg2):
        """
        Update the arg counts for the given rel  where arg1 and arg2 are strings 
        """
        ESEP = "_ESEP_"
        CSEP = "_CSEP_"
        arg1_ents = arg1.split(ESEP)
        arg2_ents = arg2.split(ESEP)
        if rel not in relgram_args:
            relgram_args[rel] = ({}, {})

        instances = relgram_args[rel]
        for ent_counts in arg1_ents:
            split = ent_counts.split(CSEP)
            ent = split[0]
            count = int(split[1])
            if ent not in instances[0]:
                instances[0][ent] = count
            else:
                instances[0][ent] += count
            
        for ent_counts in arg2_ents:
            split = ent_counts.split(CSEP)
            ent = split[0]
            count = int(split[1])
            if ent not in instances[1]:
                instances[1][ent] = count
            else:
                instances[1][ent] += count

        return relgram_args



        
    def fromRelgramCountData(self, filename, maxWindow = 10):
        """
        Create a RelgramCounter from RelgramCountData (output from RelgramExtractorScoobiApp Scala application)
        """
        relgram_map = {}
        relgram_args = {}
        RG_SEP = "_RG_SEP_"
        RGC_SEP = "_RGC_SEP_"
        with open(filename, 'r') as infile:
            for line in infile:
                first_split = line.split(RG_SEP)
                second_split = first_split[1].split(RGC_SEP)

                rel1_splits = first_split[0].split('\t')
                rel1_tup = self.convertToType(rel1_splits[0:3])
                rel1 = "|".join(rel1_tup)

                rel1_arg1 = rel1_splits[6]
                rel1_arg2 = rel1_splits[7]

                #print(rel1)
                rel2_splits = second_split[0].split('\t')
                rel2_tup = self.convertToType(rel2_splits[0:3])
                rel2 = "|".join(rel2_tup)


                rel2_arg1 = rel2_splits[6]
                rel2_arg2 = rel2_splits[7]

                relgram_args = self.updateArgCounts(relgram_args, rel1, rel1_arg1, rel1_arg2)
                relgram_args = self.updateArgCounts(relgram_args, rel2, rel2_arg1, rel2_arg2)
#                if rel1 not in relgram_args:
#                    relgram_args[rel1] = [rel1_arg1, rel1_arg2]
#                elif relgram_args[rel1] != [rel1_arg1, rel1_arg2]:
#                    print("OLD: {}, NEW: {}".format(relgram_args[rel1],[rel1_arg1, rel1_arg2]))

#                if rel2 not in relgram_args:
#                    relgram_args[rel2] = [rel2_arg1, rel2_arg2]
#                elif relgram_args[rel2] != [rel2_arg1, rel2_arg2]:
#                    print("OLD: {}, NEW: {}".format(relgram_args[rel2],[rel2_arg1, rel2_arg2]))

                if rel1 not in relgram_map:
                    relgram_map[rel1] = {} #create new entry in relgram map

                if rel2 not in relgram_map:
                    relgram_map[rel2] = {}

                stat_map = relgram_map[rel1]


                if rel2 not in stat_map: #init to ktuple of zeros if not in
                    stat_map[rel2] = np.zeros(maxWindow, dtype=np.int)

                stats = stat_map[rel2]
                #countdata = splits[8].split(',')
                countdata = second_split[1].split(',')
                for i in countdata:
                    data = i.split(':')
                    start_window = int(data[0])
                    count = int(data[1])
                    
                    start_index = start_window - 1
                    for index in range(start_index, maxWindow): #increment counts
                        stats[index] += count
        return RelgramCounter(list(range(1,maxWindow+1)), True, relgram_map, relgram_args)


    def fromRelgramCountDataOld(self, filename, maxWindow = 10):
        """
        Create a RelgramCounter from RelgramCountData (output from RelgramsLocalApp Scala application)
        """
        relgram_map = {}
        with open(filename, 'r') as infile:
            for line in infile:
                splits = line.split('\t')
                rel1 = "|".join(splits[0:3])

                print(rel1)
                if rel1 not in relgram_map:
                    relgram_map[rel1] = {} #create new entry in relgram map

                stat_map = relgram_map[rel1]

                rel2 = "|".join(splits[3:6])

                if rel2 not in stat_map: #init to ktuple of zeros if not in
                    stat_map[rel2] = np.zeros(maxWindow, dtype=np.int)

                stats = stat_map[rel2]
                countdata = splits[8].split(',')
                for i in countdata:
                    data = i.split(':')
                    start_window = int(data[0])
                    count = int(data[1])
                    
                    start_index = start_window - 1
                    for index in range(start_index, maxWindow): #increment counts
                        stats[index] += count
        return RelgramCounter(list(range(1,maxWindow+1)), True, relgram_map)



