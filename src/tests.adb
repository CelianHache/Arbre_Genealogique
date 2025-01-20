with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Arbre_Bin;
with Arbre_Genealog; use Arbre_Genealog;
with Personne; use Personne;

procedure tests is

   procedure initialise_arbre (Family_Root : in out T_Arbre_Personnes) is
      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20 : T_Personne;
   begin
      Initialise(P1, "Antoine", "Gouzy", "Homme", Time_Of(1980, 4, 14), "Libourne");
      Initialise(P2, "Marie", "Gouzy", "Femme", Time_Of(1985, 6, 21), "Libourne");
      Initialise(P3, "Jean", "Gouzy", "Homme", Time_Of(2005, 2, 15), "Libourne");
      Initialise(P4, "Sophie", "Gouzy", "Femme", Time_Of(2007, 8, 25), "Libourne");
      Initialise(P5, "Luc", "Gouzy", "Homme", Time_Of(2009, 1, 30), "Libourne");
      Initialise(P6, "Claire", "Gouzy", "Femme", Time_Of(2012, 3, 10), "Libourne");
      Initialise(P7, "David", "Gouzy", "Homme", Time_Of(2015, 5, 22), "Libourne");
      Initialise(P8, "Alice", "Gouzy", "Femme", Time_Of(1987, 11, 5), "Libourne");
      Initialise(P9, "Philippe", "Gouzy", "Homme", Time_Of(1990, 3, 30), "Libourne");
      Initialise(P10, "Julien", "Gouzy", "Homme", Time_Of(1993, 7, 14), "Libourne");
      Initialise(P11, "Martine", "Gouzy", "Femme", Time_Of(1996, 12, 17), "Libourne");
      Initialise(P12, "Nicolas", "Gouzy", "Homme", Time_Of(1999, 4, 21), "Libourne");
      Initialise(P13, "Isabelle", "Gouzy", "Femme", Time_Of(2002, 9, 6), "Libourne");
      Initialise(P14, "Thomas", "Gouzy", "Homme", Time_Of(2004, 10, 9), "Libourne");
      Initialise(P15, "Emilie", "Gouzy", "Femme", Time_Of(2006, 2, 28), "Libourne");
      Initialise(P16, "Vincent", "Gouzy", "Homme", Time_Of(2008, 5, 17), "Libourne");
      Initialise(P17, "Camille", "Gouzy", "Femme", Time_Of(2010, 8, 23), "Libourne");
      Initialise(P18, "Louis", "Gouzy", "Homme", Time_Of(2013, 1, 10), "Libourne");
      Initialise(P19, "Elise", "Gouzy", "Femme", Time_Of(2016, 6, 15), "Libourne");
      Initialise(P20, "Pierre", "Gouzy", "Homme", Time_Of(2018, 4, 2), "Libourne");

      Create_Family_Tree (Family_Root, P1);
      Add_Father (Family_Root, P2);             --01
      Add_Mother (Family_Root, P3);             --02
      Add_Father (Family_Root, P4,     "01");   --011
      Add_Mother (Family_Root, P5,     "01");   --012
      Add_Father (Family_Root, P6,     "02");   --021
      Add_Mother (Family_Root, P7,     "02");   --022
      Add_Father (Family_Root, P8,     "011");  --0111
      Add_Mother (Family_Root, P9,     "011");  --0112
      Add_Father (Family_Root, P10,    "012");  --0121
      Add_Mother (Family_Root, P11,    "012");  --0122
      Add_Father (Family_Root, P12,    "0111"); --01111
      Add_Mother (Family_Root, P13,    "0111"); --01112
      Add_Father (Family_Root, P14,    "0112"); --01121
      Add_Mother (Family_Root, P15,    "0112"); --01122
      Add_Father (Family_Root, P16,    "01111");--011111
      Add_Mother (Family_Root, P17,    "01112");--011112
      Add_Father (Family_Root, P18,    "01121");--011211
      Add_Mother (Family_Root, P19,    "01122");--011222
      Add_Father (Family_Root, P20,    "0121"); --01211
   end initialise_arbre;

   procedure test_arbre_minimal is
      Family_Root : T_Arbre_Personnes;
      Personne : T_Personne;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Initialise(Personne, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Create_Family_Tree(Family_Root, Personne);
      pragma Assert(not Is_Null (Family_Root));
      pragma Assert(Is_Null (Get_Left(Family_Root)));
      pragma Assert(Is_Null (Get_Right(Family_Root)));
      Put_Line("Test_arbre_minimal => OK");
   end test_arbre_minimal;

   procedure test_ajout_parent is 
      Family_Root : T_Arbre_Personnes;
      Antoine: T_Personne;
      Mother: T_Personne;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Initialise(Antoine, "Antoine", "Gouzy", "Homme", Ada.Calendar.Time_Of(2003, 4, 14), "Libourne");
      Initialise(Mother, "Mamam", "Gouzy", "Femme", Ada.Calendar.Time_Of(1973, 5, 15), "Libourne");
      Create_Family_Tree(Family_Root, Antoine);
      pragma Assert(not Is_Null (Family_Root));
      Add_Mother (Family_Root, Mother);
      pragma Assert(Is_Null (Get_Father(Family_Root)));
      pragma Assert(not Is_Null (Get_Mother(Family_Root)));
      Add_Father (Family_Root, Mother, "02");
      pragma Assert(not Is_Null (Get_Node_By_Id(Family_Root, "021")));
      Put_Line("Test_ajout_parent => OK");
   end test_ajout_parent;

   procedure test_ancestors_generation is       
      Family_Root : T_Arbre_Personnes;
      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11: T_Personne;
      Ancestors_List_0 : Ancestor_Array(1 .. 1); 
      Expected_Ancestors_List_0 : Ancestor_Array(1 .. 1); 
      Ancestors_List_1 : Ancestor_Array(1 .. 2); 
      Expected_Ancestors_List_1 : Ancestor_Array(1 .. 2); 
      Ancestors_List_2 : Ancestor_Array(1 .. 4); 
      Expected_Ancestors_List_2 : Ancestor_Array(1 .. 4); 
      Ancestors_List_3 : Ancestor_Array(1 .. 8); 
      Expected_Ancestors_List_3 : Ancestor_Array(1 .. 8); 
   begin
      pragma Assert(Is_Null(Family_Root));  
      initialise_arbre(Family_Root);
      P1 := Get_Value(Get_Node_By_Id (Family_Root, "0"));
      P2 := Get_Value(Get_Node_By_Id (Family_Root, "01"));
      P3 := Get_Value(Get_Node_By_Id (Family_Root, "02"));
      P4 := Get_Value(Get_Node_By_Id (Family_Root, "011"));
      P5 := Get_Value(Get_Node_By_Id (Family_Root, "012"));
      P6 := Get_Value(Get_Node_By_Id (Family_Root, "021"));
      P7 := Get_Value(Get_Node_By_Id (Family_Root, "022"));
      P8 := Get_Value(Get_Node_By_Id (Family_Root, "0111"));
      P9 := Get_Value(Get_Node_By_Id (Family_Root, "0112"));
      P10 := Get_Value(Get_Node_By_Id (Family_Root, "0121"));
      P11 := Get_Value(Get_Node_By_Id (Family_Root, "0122"));
      Ancestors_List_0 := Get_Ancestors_Generation (Family_Root, 0);
      Expected_Ancestors_List_0(1) := P1;
      pragma Assert(Equals(Expected_Ancestors_List_0, Ancestors_List_0)); 
      Ancestors_List_1 := Get_Ancestors_Generation (Family_Root, 1);
      Expected_Ancestors_List_1(1) := P2;
      Expected_Ancestors_List_1(2) := P3;
      pragma Assert(Equals(Expected_Ancestors_List_1, Ancestors_List_1)); 
      Ancestors_List_2 := Get_Ancestors_Generation (Family_Root, 2);
      Expected_Ancestors_List_2(1) := P4;
      Expected_Ancestors_List_2(2) := P5;
      Expected_Ancestors_List_2(3) := P6;
      Expected_Ancestors_List_2(4) := P7;
      pragma Assert(Equals(Expected_Ancestors_List_2, Ancestors_List_2)); 
      Ancestors_List_3 := Get_Ancestors_Generation (Family_Root, 3); 
      Expected_Ancestors_List_3(1) := P8;
      Expected_Ancestors_List_3(2) := P9;
      Expected_Ancestors_List_3(3) := P10;
      Expected_Ancestors_List_3(4) := P11;
      pragma Assert(Equals(Expected_Ancestors_List_3, Ancestors_List_3));  
      Put_Line("Test_ancestors_generation => OK");    
   end test_ancestors_generation;

   procedure test_comptage_ancetres is
      Family_Root : T_Arbre_Personnes;
      Ancestors_Count : Integer := 0;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Ancestors_Count := Count_Ancestors(Family_Root, "0");
      pragma Assert(0 = Ancestors_Count);

      initialise_arbre (Family_Root);

      -- Test comptage des ancetres sur l'arbre complet
      Ancestors_Count := Count_Ancestors(Family_Root, "0");
      pragma Assert(20 = Ancestors_Count);

      -- Test comptage des ancetres sur un noeud donné
      -- 0112 => 5
      Ancestors_Count := Count_Ancestors(Family_Root, "0112");
      pragma Assert(5 = Ancestors_Count);

      Put_Line("Test_comptage_ancetres => OK");
      
   end test_comptage_ancetres;

   procedure test_number_of_parents is 
      Family_Root : T_Arbre_Personnes;
      Two_Parents: Ancestor_Array(1 .. 2**5); 
      One_Parent: Ancestor_Array(1 .. 2**5); 
      Without_Parent: Ancestor_Array(1 .. 2**5); 
      Expected_Two_Parents: Ancestor_Array(1 .. 2**5); 
      Expected_One_Parent: Ancestor_Array(1 .. 2**5); 
      Expected_Without_Parent: Ancestor_Array(1 .. 2**5); 
      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20 : T_Personne;
   begin
      initialise_arbre (Family_Root);
      P1 := Get_Value(Get_Node_By_Id (Family_Root, "0"));
      P2 := Get_Value(Get_Node_By_Id (Family_Root, "01"));
      P3 := Get_Value(Get_Node_By_Id (Family_Root, "02"));
      P4 := Get_Value(Get_Node_By_Id (Family_Root, "011"));
      P5 := Get_Value(Get_Node_By_Id (Family_Root, "012"));
      P6 := Get_Value(Get_Node_By_Id (Family_Root, "021"));
      P7 := Get_Value(Get_Node_By_Id (Family_Root, "022"));
      P8 := Get_Value(Get_Node_By_Id (Family_Root, "0111"));
      P9 := Get_Value(Get_Node_By_Id (Family_Root, "0112"));
      P10 := Get_Value(Get_Node_By_Id (Family_Root, "0121"));
      P11 := Get_Value(Get_Node_By_Id (Family_Root, "0122"));
      P12 := Get_Value(Get_Node_By_Id (Family_Root, "01111"));
      P13 := Get_Value(Get_Node_By_Id (Family_Root, "01112"));
      P14 := Get_Value(Get_Node_By_Id (Family_Root, "01121"));
      P15 := Get_Value(Get_Node_By_Id (Family_Root, "01122"));
      P16 := Get_Value(Get_Node_By_Id (Family_Root, "011111"));
      P17 := Get_Value(Get_Node_By_Id (Family_Root, "011122"));
      P18 := Get_Value(Get_Node_By_Id (Family_Root, "011211"));
      P19 := Get_Value(Get_Node_By_Id (Family_Root, "011222"));
      P20 := Get_Value(Get_Node_By_Id (Family_Root, "01211"));
      
      Two_Parents := Nodes_With_Two_Parents (Family_Root);
      One_Parent := Nodes_With_Only_One_Parent (Family_Root);
      Without_Parent := Nodes_Without_Parent (Family_Root);
      Expected_Two_Parents(1) := P1;
      Expected_Two_Parents(2) := P2;
      Expected_Two_Parents(3) := P3;
      Expected_Two_Parents(4) := P4;
      Expected_Two_Parents(5) := P5;
      Expected_Two_Parents(6) := P8;
      Expected_Two_Parents(7) := P9;
      Expected_One_Parent(1) := P10;
      Expected_One_Parent(2) := P12;
      Expected_One_Parent(3) := P13;
      Expected_One_Parent(4) := P14;
      Expected_One_Parent(5) := P15;
      Expected_Without_Parent(1) := P6;
      Expected_Without_Parent(2) := P7;
      Expected_Without_Parent(3) := P11;
      Expected_Without_Parent(4) := P16;
      Expected_Without_Parent(5) := P17;
      Expected_Without_Parent(6) := P18;
      Expected_Without_Parent(7) := P19;
      Expected_Without_Parent(8) := P20;
      pragma Assert(Equals(Two_Parents, Expected_Two_Parents));
      pragma Assert(Equals(One_Parent, Expected_One_Parent));
      pragma Assert(Equals(Without_Parent, Expected_Without_Parent));
      Put_Line("Test_number_of_parents => OK");
   end test_number_of_parents;

begin

   test_arbre_minimal;
   test_ajout_parent;
   test_ancestors_generation;
   test_comptage_ancetres;
   test_number_of_parents;
end tests;