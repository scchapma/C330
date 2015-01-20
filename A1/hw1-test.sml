(* Steve Chapman, #V00190898 *)

(* CSC 330 *)
(* Assignment 1 Simple Test *)
(* January 16, 2015 *)

(* These are basic test cases.  *)

(* loads the bindings from the file with your solutions *)

(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

(* Some bindings to avoid repetition *)

val feb28_2012 = {year = 2012, month = 2, day = 28}
val feb27_2012 = {year = 2012, month = 2, day = 27}		     
val dec1_2013 = {year = 2013, month = 12, day = 1}
val march31_2011 = {year = 2011, month = 3, day = 31}
val april28_2011 = {year = 2011, month = 4, day = 28}
val june1_2013 = {year = 2013, month = 6, day = 1}		     

val test1 = is_older({year = 1, month = 2, day = 3},{year = 2, month = 3, day = 4}) = false
val test1a = is_older(feb28_2012,dec1_2013) = false;
val test1b = is_older(dec1_2013, feb28_2012) = true;
val test1c = is_older(dec1_2013, dec1_2013) = false;
val test1d = is_older(march31_2011, april28_2011) = false;

val test2 = number_in_month([feb28_2012,dec1_2013],2) = 1
val test2a = number_in_month([feb28_2012,dec1_2013],3) = 0
val test2b = number_in_month([feb28_2012,dec1_2013,march31_2011,april28_2011],3) = 1
val test2c = number_in_month([feb28_2012, {year = 2012, month = 2, day = 25}], 2) = 2
										     
val test3a = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = 3
val test3b = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,5,6]) = 1
val test3c = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[5,6,7]) = 0				      

val test4 = dates_in_month([feb28_2012,dec1_2013],2) = [feb28_2012]
val test4a = dates_in_month([feb28_2012,dec1_2013],12) = [dec1_2013]
val test4b = dates_in_month([feb28_2012,dec1_2013],3) = []
val test4c = dates_in_month([feb28_2012,feb27_2012],2) = [feb28_2012, feb27_2012]							    

val test5a = dates_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = [feb28_2012,march31_2011,april28_2011]
val test5b = dates_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[5,7]) = []
val test5c = dates_in_months([feb27_2012,feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = [feb27_2012,feb28_2012,march31_2011,april28_2011]
													
val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test6a = get_nth(["hi", "there", "how", "are", "you"], 7) = "never" handle InvalidParameter => true
val test6b = get_nth(["hi", "there", "how", "are", "you"], 0) = "never" handle InvalidParameter => true
val test6c = get_nth([], 0) = "never" handle InvalidParameter => true
val test6d = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val test6e = get_nth(["hi", "there", "how", "are", "you"], 5) = "you"
								   
val test7 = date_to_string(june1_2013) = "June 1, 2013"
val test7a = date_to_string(april28_2011) = "April 28, 2011"
val test7b = date_to_string({year = 2012, month = 12, day = 31}) = "December 31, 2012"
val test7c = date_to_string({year = 2012, month = 1, day = 1}) = "January 1, 2012"								      
val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum(10, [11,1,2,3,4,5]) = 0
val test8b = number_before_reaching_sum(12, [11,1,2,3,4,5]) = 1
val test8c = number_before_reaching_sum(1, [1,2,3,4,5]) = 0;
val test8d = number_before_reaching_sum(6, [1,2,3,4,5]) = 2;
val test8e = number_before_reaching_sum(16, [1,2,3,4,5]) = 100 handle InvalidParameter => true

val test9  = what_month(70) = 3
val test9a = what_month(31) = 1
val test9b = what_month(32) = 2
val test9c = what_month(360) = 12
val test9d = what_month(1) = 1
val test9e = what_month(365) = 12

val test10 = month_range(31, 34) = [1,2,2,2]
val test10a = month_range(360, 365) = [12,12,12,12,12,12];
val test10b = month_range(31,31 + 28 +1) = [1,
                                           2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                           3]
val test10c = month_range(35, 34) = []
val test10d = month_range(35, 35) = [2]
val test10e = month_range(31+29, 31+29) = [3]
val test10f = month_range(31+28+30, 31+28+31+2) = [3,3,4,4]					      

val test11 = oldest([feb28_2012,march31_2011,april28_2011]) = SOME feb28_2012
val test11a = oldest([april28_2011]) = SOME april28_2011
val test11b = oldest([]) = NONE
val test11c = oldest([march31_2011,march31_2011,march31_2011]) = SOME march31_2011			       

val test12 = reasonable_date({year = 2014, month = 12, day = 31});
val test12a = not (reasonable_date {year = 2015, month = 2, day = 29});
val test12b = reasonable_date({year = 2012, month = 2, day = 29});
val test12c = not (reasonable_date{year = 2014, month = 0, day = 31});
val test12d = not (reasonable_date{year = 2014, month = 13, day = 31});
val test12e = reasonable_date({year = 2000, month = 2, day = 29});
val test12f = not (reasonable_date{year = 2100, month = 2, day = 29});
val test12g = not (reasonable_date{year = 2015, month = 2, day = 29});
