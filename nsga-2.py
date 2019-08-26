import math
def exec(values1, values2):
    

    def fast_non_dominated_sort(values1, values2):
        S=[[] for i in range(0,len(values1))]
        front = [[]]
        n=[0 for i in range(0,len(values1))]
        rank = [0 for i in range(0, len(values1))]

        for p in range(0,len(values1)):
            S[p]=[]
            n[p]=0
            for q in range(0, len(values1)):
                if (values1[p] < values1[q] and values2[p] <values2[q]) or (values1[p] <= values1[q] and values2[p] < values2[q]) or (values1[p] < values1[q] and values2[p] <= values2[q]):
                    if q not in S[p]:
                        S[p].append(q)
                elif (values1[q] < values1[p] and values2[q] < values2[p]) or (values1[q] <= values1[p] and values2[q] < values2[p]) or (values1[q] < values1[p] and values2[q] <= values2[p]):
                    n[p] = n[p] + 1
            if n[p]==0:
                rank[p] = 0
                if p not in front[0]:
                    front[0].append(p)

        i = 0
        while(front[i] != []):
            Q=[]
            for p in front[i]:
                for q in S[p]:
                    n[q] =n[q] - 1
                    if( n[q]==0):
                        rank[q]=i+1
                        if q not in Q:
                            Q.append(q)
            i = i+1
            front.append(Q)

        del front[len(front)-1]
        return front

    # sort_points_in each layers based on y1 or y2
    def sort_points_in_layers(layers,f1,f2):
        index_map = []
        sorted_layers={}
        for i in range(len(layers)):
            points={}
            index=[]
            for j in (layers[i]):
                points[f1[j]]=f2[j]
                index.append(j)
            sorted_layers[i] = sorted(points.items())
            list1, list2 = list(zip(*sorted(zip(points.items(), index))))
            index_map.append(list2)
        return (sorted_layers,index_map)


    #Function to calculate crowding distance
    def crowding_distance(sorted_layers):
        each_layer_dist={}
        index_layer={}
        each_point_dist={}
        index_map={}
        for i in sorted_layers.keys():
            distance = [0 for j in range(0,len(sorted_layers[i]))] 
            distance[0] = math.inf
            distance[len(sorted_layers[i])-1] = math.inf
            each_layer_dist[i] = distance
            for k in range(1,len(sorted_layers[i])-1):
                for j in range(2):
                    distance[k] = distance[k] + abs(sorted_layers[i][k+1][j] - sorted_layers[i][k-1][j])
                each_layer_dist[i][k] = distance[k]
        return each_layer_dist

    def sort_crowding(crowding , index_map):
        index = []
        for i in range(len(crowding)):
            list1, list2= list(zip(*sorted(zip(crowding[i], index_map[i]),reverse=True)))
            index.append(list2)
        return(index)


    layers = fast_non_dominated_sort(values1,values2)
    sorted_layers,index_map = sort_points_in_layers(layers,values1,values2)
    crowding = crowding_distance(sorted_layers)
    indices = sort_crowding(crowding,index_map)

    return indices