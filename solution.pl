% bengisu takkin
% 2018400036
% compiling: yes
% complete: yes

% include the knowledge base
:- encoding(utf8).
:- ['load.pro'].

%subtracting [i]th elements of the two lists
subtract_two_list([],[],[]).
subtract_two_list([H1|T1],[H2|T2],[Head|TailResult]):-
    subtract_two_list(T1,T2,TailResult),
    ((not(H1 is -1),
    not(H2 is -1),
    Head is H1-H2,!);Head is 0).

%taking the square of the all elements in a list one by one
square_list([],[]).
square_list([H|T],[Head|Tail]):-
    square_list(T,Tail),
    Head is H*H.

%summing all the elements in the list, returns the sum
sum_list([],0).       
sum_list([Head|Tail],Sum):-
    sum_list(Tail,TailSum),
    Sum is Head+TailSum.

%Calculates glanian distance
glanian_distance(Name1, Name2, Distance):-  %Name2 is target
expects(Name1,_,List1),  %expected features of Name1
glanian(Name2,_,List2),    %features of Name2
subtract_two_list(List1,List2,ResultList),    %subtracting two lists
square_list(ResultList,SquaredList),     %taking the square of the elements
sum_list(SquaredList,Sum),      %summing elements
Distance is sqrt(Sum).          %taking square root

%multiplying [i]th elements of the two list
multiply_two_list([],[],[]).
multiply_two_list([H1|T1],[H2|T2],[Head|TailList]):-
    multiply_two_list(T1,T2,TailList),
    ((not(H1 is -1),Head is H1*H2,!);Head is 0).   %if an element equals to -1, the result becomes 0

%Calculates weighted glanian distance
weighted_glanian_distance(Name1, Name2, Distance):-
    expects(Name1,_,List1), glanian(Name2,_,List2),
    subtract_two_list(List1,List2,SubtractedList),
    square_list(SubtractedList,SquaredList),weight(Name1,WeightList),
    multiply_two_list(WeightList,SquaredList,MultipliedList),  %as an extra step, it multiplies the results with weights of Name1
    sum_list(MultipliedList,Sum),   
    Distance is sqrt(Sum).

%finds current city of the Name and returns it
current_city(Name,CurrentCity):-
    city(CityName,Habitants,_),
    member(Name, Habitants),
    CurrentCity=CityName,!.

%returns a list that includes current city and the liked cities
find_possible_cities(Name, [Head|Tail]):-
    current_city(Name,CurrentCity),  %finding current city
    Head=CurrentCity,
    likes(Name,_,LikedCities),  %getting liked cities
    Tail=LikedCities.

%merging two lists, returns merged list
merge([],List,List).
merge([Head|Tail],CityList2,[HeadMerged|TailMerged]):-
    (not(member(Head,CityList2)),
    HeadMerged=Head,    %if not includes the member, add to list
    merge(Tail,CityList2,TailMerged),!);
    (merge(Tail,CityList2,[HeadMerged|TailMerged])).

%getting unification of possible cities of two glanians
merge_possible_cities(Name1, Name2, MergedCities):-
    find_possible_cities(Name1,CityList1),
    find_possible_cities(Name2,CityList2),
    merge(CityList1,CityList2,Merged),
    remove_dups(Merged, MergedCities),!.

%getting a list of common elements of the two list
common_elements([],_,[]).
common_elements([Head|Tail],List2,[Head|TailCommon]):-
    member(Head,List2),!,       %if element is a member
    common_elements(Tail,List2,TailCommon).
common_elements([_|Tail],List2,TailCommon):-
    common_elements(Tail,List2,TailCommon).

%finding mutual liked activities of two glanians
find_mutual_activities(Name1,Name2,ActivityList):-
    likes(Name1,Activities1,_),
    likes(Name2,Activities2,_),
    common_elements(Activities1,Activities2,ActivityList).

%getting the list of all glanian names
all_glanian(TargetList) :-
    findall(Name, glanian(Name,_,_),TargetList).

%takes a dashed list, divides it into two lists                                                   
divide_dashed_list([],[],[]).
divide_dashed_list([[H|T]|Tail],[H1|T1],[H2|T2]):-
    H1=H,H2=T,divide_dashed_list(Tail,T1,T2).

%get a list of targets which has the expected genders of the name, make a list with target and its distance to Name
get_target_list(_,[],_,[]).
get_target_list(_,_,[],[]).
get_target_list(Name1,[Name2|OtherNames],ExpectedGenders,[[Distance|TargetName]|Tail]):-
    not(Name1=Name2),  %target should not be name itself
    glanian(Name2,Gender,_),
    member(Gender,ExpectedGenders), %target should has an expected gender
	TargetName=Name2,
    glanian_distance(Name1,Name2,Distance),!,
    get_target_list(Name1,OtherNames,ExpectedGenders,Tail).
get_target_list(Name1,[_|OtherNames],ExpectedGenders,List):-
    get_target_list(Name1,OtherNames,ExpectedGenders,List).  

%finding possible targets with distances for name
find_possible_targets(Name, Distances, TargetList):-
    expects(Name,ExpectedGenders,_),
    all_glanian(Targets),
    get_target_list(Name,Targets,ExpectedGenders,List),
    sort(List,Sorted),
    divide_dashed_list(Sorted,Distances,TargetList),!.

%similar with the method get_target_list, It returns a dashed list of targets with their weighted distances
get_weighted_list(_,[],_,[]).
get_weighted_list(_,_,[],[]).
get_weighted_list(Name1,[Name2|OtherNames],ExpectedGenders,[[WeightedDistance|TargetName]|Tail]):-
    not(Name1=Name2),  %If the target is not the name itself
    glanian(Name2,Gender,_),
    member(Gender,ExpectedGenders),  %If the target has an expected gender
	TargetName=Name2,
    weighted_glanian_distance(Name1,Name2,WeightedDistance),!,  %get weighted distance
    get_weighted_list(Name1,OtherNames,ExpectedGenders,Tail).
get_weighted_list(Name1,[_|OtherNames],ExpectedGenders,List):-
    get_weighted_list(Name1,OtherNames,ExpectedGenders,List).  

%finds possible targets with weighted distances for given name
find_weighted_targets(Name, Distances, TargetList):-
	expects(Name,ExpectedGenders,_),
    all_glanian(Targets),
    get_weighted_list(Name,Targets,ExpectedGenders,List),
    sort(List,Sorted),
    divide_dashed_list(Sorted,Distances,TargetList),!.

%checks whether the given number is in between the given limits
in_limits(Feature,[Min,Max]):-
    Feature>Min,Feature<Max.

%checks whether a target is a tolerable for a glanian
is_tolerable([],_).
is_tolerable([Feature|Other],[Limits|OtherLimits]):-
    (Limits=[];in_limits(Feature,Limits)),!,  %checks the features of the glanian has valid values, between given limits of the glanian
    is_tolerable(Other,OtherLimits).

%gets a list of conflictions of liked activities of the target and disliked activities of the glanian
activity_conflicts(_,[],Count):-Count is 0.
activity_conflicts(Disliked,[Liked|OtherLiked],Count):-
    not(member(Liked,Disliked)),!,activity_conflicts(Disliked,OtherLiked,NewCount),Count is NewCount.
activity_conflicts(Disliked,[_|OtherLiked],Count):-
	activity_conflicts(Disliked,OtherLiked,NewCount),Count is NewCount+1.

%gets a list of targets with some specific features
best_target_list(_,[],_,[]).
best_target_list(_,_,[],[]).
best_target_list(Name1,[Name2|OtherNames],ExpectedGenders,[Target|OtherTargets]):-
    not(Name1=Name2),   %target is not the glanian itself
    glanian(Name2,Gender,Features),
    member(Gender,ExpectedGenders), %target's gender has an expected gender for glanian
    not(old_relation([Name1,Name2])),  %the target and the glanian has not an old relation
    not(old_relation([Name2,Name1])),
    dislikes(Name1,DislikedActivities,_,Tolerance),
    is_tolerable(Features,Tolerance),   %if the targets features are in between tolerablity limits of the glanian
    likes(Name2,LikedActivities,_),
    activity_conflicts(DislikedActivities,LikedActivities,Count),
    Count<3,  %if the glanians disliked activities and targets liked activities has no conflicts more than 2 
    Target=Name2,!,  %then add the target to the list
    best_target_list(Name1,OtherNames,ExpectedGenders,OtherTargets).  %repeat them for other targets
best_target_list(Name1,[_|OtherNames],ExpectedGenders,List):-
    best_target_list(Name1,OtherNames,ExpectedGenders,List). 

%gets the list of glanians with special features for another glanian
 get_best_target_list(Name,List):-all_glanian(Targets),
    expects(Name,Gender,_),best_target_list(Name,Targets,Gender,List),!.
   
%gets number of appropriate activities
has_activity([],_,HasActivity):-HasActivity is 0.
has_activity([HeadActivity|Other],ActivityList,HasActivity):-
    member(HeadActivity,ActivityList),!,has_activity(Other,ActivityList,NewHasActivity),
    HasActivity is NewHasActivity+1.
has_activity([_|Other],ActivityList,HasActivity):-
    has_activity(Other,ActivityList,HasActivity).
 
%finds a list of cities among all cities which has at least one common activity with selected activities
city_with_activity([],_,[]).
city_with_activity([CityName|OtherCities],ActivityList,[Head|Tail]):-
    city(CityName,_,CityActivities), %get activities
    has_activity(CityActivities,ActivityList,HasActivity),  %gets common activities of the city with given list
    HasActivity>0,!,  %checks whether there is at least one common activity
    Head=CityName,
    city_with_activity(OtherCities,ActivityList,Tail),!.
city_with_activity([_|OtherCities],ActivityList,List):-
 	city_with_activity(OtherCities,ActivityList,List ).
    
%returns a list of cities which is an element of find_possible_cities or a city that Name like, and element of merge_possible_cities and 
%not a member of disliked cities for specific glanian
best_city_list(_,_,[],[]).
best_city_list(Name,Target,[City|OtherCities],[CityHead|CityTail]):-%finding list of cities for couple
    find_possible_cities(Name,PossibleCityList), %3.3
    findall(C, city(C,_,_),AllCities),  %all cities
    likes(Name,ActivityList,_),  %get ActivityList
    dislikes(Name,_,DislikedCities,_),
    merge_possible_cities(Name,Target,MergePossibleCitylist),  %3.4
    city_with_activity(AllCities,ActivityList,CitiesWithActivities),   %city with a liked activity
    ((((member(City,PossibleCityList));member(City,CitiesWithActivities)),not(member(City,DislikedCities)),member(City,MergePossibleCitylist))->(CityHead=City,
    best_city_list(Name,Target,OtherCities,CityTail))),!.
best_city_list(Name,Target,[_|OtherCities],CityList):-
    best_city_list(Name,Target,OtherCities,CityList),!.

%takes list of possible activities
%returns list with quaternary elements with [wightedDistance,target,city,activity] structure
dashed_list_activity_dimension(_,_,_,[],[]).
dashed_list_activity_dimension(Name,TheTarget,TheCity,[Activity|OtherActivities],[Head|Tail]):-
	weighted_glanian_distance(Name, TheTarget, WeightedDistance),  %get distance
    Head=[WeightedDistance, TheTarget, Activity, TheCity],
    dashed_list_activity_dimension(Name,TheTarget,TheCity,OtherActivities,Tail),!.

%Takes a match and list of possible cities as an input, gets list of possible activities for each city,
%Make dashed lists for each city and returns all as a single dashed list.
dashed_list_city_dimension(_,_,[],[]).
dashed_list_city_dimension(Name,Target,[City|OtherCities],[Head|Tail]):-
    city(City,_,CityActivityList),   %gets possible activities
    likes(Name,LikedActivities,_),
    dislikes(Name,DislikedActivities,_,_),
    find_possible_cities(Name,PossibleCityList),
    findall(A,(member(A,CityActivityList),not(member(A,DislikedActivities)),
    ((not(LikedActivities=[])->member(A,LikedActivities);true);member(City,PossibleCityList))),Activities), 
    dashed_list_activity_dimension(Name,Target,City,Activities,DashedList),  %gets list with quaternary elements with [distance,target,city,activity] structure
    Head=DashedList,
    dashed_list_city_dimension(Name,Target,OtherCities,Tail),!.

%Takes list of possible targets as an input, for each match, gets list of possible cities,
%Then gives it dashed_list_city and gets a dashed list consist of possible cities and activities for that cities for each match
%Returns another dashed list consist of dashed lists for all matches
dashed_list_target_dimension(_,[],[]).
dashed_list_target_dimension(Name,[Target|OtherTargets],[Head|Tail]):-
    findall(C, city(C,_,_),AllCities),  %all cities
    best_city_list(Name,Target,AllCities,BestCityListForCouple), %get possible cities
    dashed_list_city_dimension(Name,Target,BestCityListForCouple,DashedList),!,
    dashed_list_target_dimension(Name,OtherTargets,Tail),!,
    Head=DashedList.

%finds all elements including inner elements of a dashed list, returns all
my_member(Element,ListofList):-
    member(List,ListofList),
    member(Element,List).

%pulls out dimensions of the given dashed list, makes one dimensional list with its elements
pull_out_dimensions(ListofList,List):-
    findall(Element,(my_member(Element,ListofList),not(Element=[])),List).

%takes a list which each element has 4 elements, divides it into four separate lists
divide_dashed_with_four([],[],[],[],[]).
divide_dashed_with_four([[Dist, Targ, Act, City]|Tail],[H1|T1],[H2|T2],[H3|T3],[H4|T4]):-
    H1=Dist,H2=Act,H3=City,H4=Targ,
    divide_dashed_with_four(Tail,T1,T2,T3,T4),!.

%finds best matching targets according to their distances, genders and returns targets with distances and list of possible cities
%with possible activities that can be done in the cities
find_my_best_target(Name, Distances, ActivityList, CityList, TargetList):-
    get_best_target_list(Name,BestTargetList),  %get possible targets  %get possible targets
    dashed_list_target_dimension(Name,BestTargetList,DashedList),  %gets a dashed list which contains all wanted lists
	pull_out_dimensions(DashedList,NewList),  %pulls out inner dimesions 
    pull_out_dimensions(NewList,List),
    sort(List,Sorted),  %sorts the list
    divide_dashed_with_four(Sorted,Distances,ActivityList, CityList, TargetList),!.  %divides the list to four other list

%takes list of possible activities, for appropriate cities for both Name1 and Name2
%returns list with quaternary elements with [avgWeightedDistance,target,city,activity] structure
best_match_activity_dimension(_,_,_,[],[]).
best_match_activity_dimension(Name1,Name2,TheCity,[Activity|OtherActivities],[Head|Tail]):-
    weighted_glanian_distance(Name1, Name2, WgdDistance1),
    weighted_glanian_distance(Name2, Name1, WgdDistance2),
    Distance is (WgdDistance1+WgdDistance2)/2, %average weighted distance
    best_match_activity_dimension(Name1,Name2,TheCity,OtherActivities,Tail),
    Head=[Distance, Name2, Activity, TheCity],!.
    %Tail=[]->List=Head;List=[Head|Tail],!.

find_the_activities(Name,Target,City,Activities):-
    find_possible_cities(Name,PossibleCityList1),
    find_possible_cities(Target,PossibleCityList2),
    city(City,_,CityActivityList),
    likes(Name,LikedActivities1,_),
    likes(Target,LikedActivities2,_),
    dislikes(Name,DislikedActivities1,_,_),
    dislikes(Target,DislikedActivities2,_,_),
    findall(A,(member(A,CityActivityList),not(member(A,DislikedActivities1)),not(member(A,DislikedActivities2)),
    ((not(LikedActivities1=[])->member(A,LikedActivities1);true);member(City,PossibleCityList1)),((not(LikedActivities2=[])->member(A,LikedActivities2);true);member(City,PossibleCityList2))),Activity),
    Activities=Activity.

remove_dups([], []).
remove_dups([First|Rest],NewRest):-
    member(First,Rest),
    remove_dups(Rest,NewRest).
remove_dups([First|Rest],[First|NewRest]):-
    not(member(First, Rest)),
    remove_dups(Rest, NewRest).

%Takes a match and list of possible cities for both glanian and its target as an input, gets list of possible activities for each city,
%Make dashed lists for each city and returns all as a single dashed list.
best_match_city_dimension(_,_,[],[]).
best_match_city_dimension(Name,Target,[City|OtherCities],[Head|Tail]):-
    find_the_activities(Name,Target,City,OutputList),
    remove_dups(OutputList,ActivityList),
    best_match_activity_dimension(Name,Target,City,ActivityList,DashedList),!,
    Head=DashedList,
    best_match_city_dimension(Name,Target,OtherCities,Tail),!.

%gets a list of possible targets by using both target and glanians preferences,
%gets a list of possible cities for each match pair and using this gets a list with quaternary elements with [avgWeightedDistance,target,city,activity] structure
best_match_target_dimension(_,[],[]).
best_match_target_dimension(Name,[Target|OtherTargets],[Head|Tail]):-
    findall(C, city(C,_,_),AllCities),  %all cities
    best_city_list(Name,Target,AllCities,CityListForName), %get possible cities
    best_city_list(Target,Name,AllCities,CityListForTarget),
    common_elements(CityListForName,CityListForTarget,CityList), 
    best_match_city_dimension(Name,Target,CityList,DashedList),
    best_match_target_dimension(Name,OtherTargets,Tail),
    Head=DashedList,!.

%takes a list of possible targets that is derived by considering Name1's preferences as an input
%via adding target's preferences, returns a list of possible matches for the glanian
best_match_list(_,[],[]):-!.
best_match_list(Name1,[Name2|OtherNames],[Target|OtherTargets]):-  %Name1 is glanian, Name2 is the head of the targetlist
    expects(Name2,ExpectedGenders,_),
    glanian(Name1,Gender,Features),
    member(Gender,ExpectedGenders),  %checks whether Name1's gender is in expected genders of Name2 which is the target
    dislikes(Name2,DislikedActivities,_,Tolerance),
    is_tolerable(Features,Tolerance), %checks whether the glanian's features are in the target's tolerable range
    likes(Name1,LikedActivities,_),
    activity_conflicts(DislikedActivities,LikedActivities,Count),  
    Count<3,    %checks activity conflicts are smaller than 3
    Target=Name2,!,
    best_match_list(Name1,OtherNames,OtherTargets).
best_match_list(Name1,[_|OtherNames],List):-
    best_match_list(Name1,OtherNames,List). 

%find a list for best matches with their average weighted distances, possible cities and possible activities
%by considering both target's and glanian's preferences
find_my_best_match(Name, Distances, ActivityList, CityList, TargetList):-
    get_best_target_list(Name,TargetsForName),  %gets possible targets for glanian using glanian's preferences
    best_match_list(Name, TargetsForName, FinalTargetList), %gets final target list via adding target's preferences
    best_match_target_dimension(Name,FinalTargetList,DashedList), %gets a quaternary dashed list
	pull_out_dimensions(DashedList,NewList), %pulls out its dimensions
    pull_out_dimensions(NewList,List),
    sort(List,Sorted),
    divide_dashed_with_four(Sorted, Distances, ActivityList, CityList, TargetList),!.   %divides quaternary list into 4 seperate lists

 %calculates distances between name and its targets
 get_weighted(_,[],[]). 
 get_weighted(Name,[Target|Other],[Head|Tail]):-
    weighted_glanian_distance(Name,Target,Distance),
    Head=[Distance,Name,Target],
    get_weighted(Name,Other,Tail),!.

%gets all targets which has preferred genders for both parties
get_proper_target(N,G1,E1,Targets):-
    setof(T,N^(glanian(T,G2,_),not(N=T),expects(T,E2,_),member(G2,E1),member(G1,E2)),Targets),!.

%gets all matches with their distances
get_matches([],[]).
get_matches([Name|Other],[Head|Tail]):-
    glanian(Name,Gender,_),expects(Name,ExpectedGenders,_),
    get_proper_target(Name,Gender,ExpectedGenders,Targets),
    get_weighted(Name,Targets,WeightedList),
    Head=WeightedList,
    get_matches(Other,Tail),!.

%gets list of elements as much as count
get_top_list(_,0,[]).
get_top_list([],_,[]).
get_top_list([Match|Others],Count,[Head|Tail]):-
    Head=Match,
    NewCount is Count-1,
    get_top_list(Others,NewCount,Tail).

%removes duplicate name pairs
remove_duplicates(OldList,List):-
    findall([Name,Target],(member([_,Name,Target],OldList),member([_,Target,Name],OldList)),Common),
	findall([Name,Target],(member([_,Name,Target],OldList),not(member([_,Target,Name],OldList))),UnCommon),
    merge(Common,UnCommon,List).

%finds 10 best matches in the all data base
ten_best_matches(BestMatches):-
    all_glanian(Glanians),   
    get_matches(Glanians,TopMatches),   %gets all appopriate matches with their weighted distances
    pull_out_dimensions(TopMatches,List),
    sort(List,Sorted),
    get_top_list(Sorted,20,Top20),
    remove_duplicates(Top20,Matches),
    get_top_list(Matches,10,BestMatches),!.

   

