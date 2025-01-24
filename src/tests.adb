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
      Initialise(P1, "Antoine", "Gouzy", "Homme", new Time'(Time_Of(1980, 4, 14)), "Libourne");
      Initialise(P2, "Marie", "Gouzy", "Femme", new Time'(Time_Of(1985, 6, 21)), "Libourne");
      Initialise(P3, "Jean", "Gouzy", "Homme", new Time'(Time_Of(2005, 2, 15)), "Libourne");
      Initialise(P4, "Sophie", "Gouzy", "Femme", new Time'(Time_Of(2007, 8, 25)), "Libourne");
      Initialise(P5, "Luc", "Gouzy", "Homme", new Time'(Time_Of(2009, 1, 30)), "Libourne");
      Initialise(P6, "Claire", "Gouzy", "Femme", new Time'(Time_Of(2012, 3, 10)), "Libourne");
      Initialise(P7, "David", "Gouzy", "Homme", new Time'(Time_Of(2015, 5, 22)), "Libourne");
      Initialise(P8, "Alice", "Gouzy", "Femme", new Time'(Time_Of(1987, 11, 5)), "Libourne");
      Initialise(P9, "Philippe", "Gouzy", "Homme", new Time'(Time_Of(1990, 3, 30)), "Libourne");
      Initialise(P10, "Julien", "Gouzy", "Homme", new Time'(Time_Of(1993, 7, 14)), "Libourne");
      Initialise(P11, "Martine", "Gouzy", "Femme", new Time'(Time_Of(1996, 12, 17)), "Libourne");
      Initialise(P12, "Nicolas", "Gouzy", "Homme", new Time'(Time_Of(1999, 4, 21)), "Libourne");
      Initialise(P13, "Isabelle", "Gouzy", "Femme", new Time'(Time_Of(2002, 9, 6)), "Libourne");
      Initialise(P14, "Thomas", "Gouzy", "Homme", new Time'(Time_Of(2004, 10, 9)), "Libourne");
      Initialise(P15, "Emilie", "Gouzy", "Femme", new Time'(Time_Of(2006, 2, 28)), "Libourne");
      Initialise(P16, "Vincent", "Gouzy", "Homme", new Time'(Time_Of(2008, 5, 17)), "Libourne");
      Initialise(P17, "Camille", "Gouzy", "Femme", new Time'(Time_Of(2010, 8, 23)), "Libourne");
      Initialise(P18, "Louis", "Gouzy", "Homme", new Time'(Time_Of(2013, 1, 10)), "Libourne");
      Initialise(P19, "Elise", "Gouzy", "Femme", new Time'(Time_Of(2016, 6, 15)), "Libourne");
      Initialise(P20, "Pierre", "Gouzy", "Homme", new Time'(Time_Of(2018, 4, 2)), "Libourne");

      Create_Family_Tree (Family_Root, P1);
      Add_Mother (Family_Root, P2);
      Add_Father (Family_Root, P3);
      Add_Father (Family_Root, P4,     "01");
      Add_Mother (Family_Root, P5,     "01");
      Add_Father (Family_Root, P6,     "02");
      Add_Mother (Family_Root, P7,     "02");
      Add_Father (Family_Root, P8,     "011");
      Add_Mother (Family_Root, P9,     "011");
      Add_Father (Family_Root, P10,    "012");
      Add_Mother (Family_Root, P11,    "012");
      Add_Father (Family_Root, P12,    "0111");
      Add_Mother (Family_Root, P13,    "0111");
      Add_Father (Family_Root, P14,    "0112");
      Add_Mother (Family_Root, P15,    "0112");
      Add_Father (Family_Root, P16,    "01111");
      Add_Mother (Family_Root, P17,    "01112");
      Add_Father (Family_Root, P18,    "01121");
      Add_Mother (Family_Root, P19,    "01122");
      Add_Father (Family_Root, P20,    "0121");
   end initialise_arbre;

   procedure test_arbre_minimal is
      Family_Root : T_Arbre_Personnes;
      Personne : T_Personne;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Initialise(Personne, "Antoine", "Gouzy", "Homme", new Time'(Time_Of(2003, 4, 14)), "Libourne");
      Create_Family_Tree(Family_Root, Personne);
      pragma Assert(not Is_Null (Family_Root));
      pragma Assert(Is_Null (Get_Left(Family_Root)));
      pragma Assert(Is_Null (Get_Right(Family_Root)));
      Display_Family_Tree (Family_Root);
   end test_arbre_minimal;

   procedure test_ajout_parent is 
      Family_Root : T_Arbre_Personnes;
      Antoine: T_Personne;
      Mother: T_Personne;
   begin
      pragma Assert(Is_Null(Family_Root)); 
      Initialise(Antoine, "Antoine", "Gouzy", "Homme", new Time'(Time_Of(2003, 4, 14)), "Libourne");
      Initialise(Mother, "Mamam", "Gouzy", "Femme", new Time'(Time_Of(1973, 5, 15)), "Libourne");
      Create_Family_Tree(Family_Root, Antoine);
      pragma Assert(not Is_Null (Family_Root));
      Add_Mother (Family_Root, Mother);
      pragma Assert(Is_Null (Get_Father(Family_Root)));
      pragma Assert(not Is_Null (Get_Mother(Family_Root)));
      Add_Father (Family_Root, Mother, "02");
      pragma Assert(not Is_Null (Get_Node_By_Id(Family_Root, "021")));
      Display_Family_Tree (Family_Root);
   end test_ajout_parent;

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

      Put_Line("test_comptage_ancetres => OK");
      
   end test_comptage_ancetres;

   procedure test_remove_node is
      Family_Root : T_Arbre_Personnes;
      Ancestors_Count : Integer := 0;
   begin
      -- Test suppression de l'arbre complet
      initialise_arbre (Family_Root);
      Ancestors_Count := Count_Ancestors(Family_Root, "0");
      pragma Assert(20 = Ancestors_Count);

      Remove_Family_Member (Family_Root, "0");

      Ancestors_Count := Count_Ancestors(Family_Root, "0");
      pragma Assert(0 = Ancestors_Count);

      -- Test suppression à partir d'un noeud donné
      initialise_arbre (Family_Root);
      Ancestors_Count := Count_Ancestors(Family_Root, "0");
      pragma Assert(20 = Ancestors_Count);

      Remove_Family_Member (Family_Root, "012");

      Ancestors_Count := Count_Ancestors(Family_Root, "0");
      pragma Assert(16 = Ancestors_Count);

      Put_Line("test_remove_node => OK");
      
   end test_remove_node;


begin

   test_arbre_minimal;
   test_ajout_parent;
   test_comptage_ancetres;
   test_remove_node;

end tests;