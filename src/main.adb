with Arbre_Bin;
with Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

   subtype Id_Type is Integer;
   subtype Element_Type is Integer;

   package Integer_Tree is new Arbre_Bin (Element_Type => Element_Type, Id_Type => Id_Type);
   use Integer_Tree;
   use Ada.Text_IO;

   Tree : T_Arbre;

   procedure Show_Menu is
   begin
      Put_Line("=== Menu ===");
      Put_Line("1. Initialiser l'arbre (racine)");
      Put_Line("2. Ajouter un nœud gauche");
      Put_Line("3. Ajouter un nœud droit");
      Put_Line("4. Supprimer le nœud gauche");
      Put_Line("5. Supprimer le nœud droit");
      Put_Line("6. Afficher l'arbre");
      Put_Line("7. Quitter");
   end Show_Menu;

   function Get_User_Input(Prompt : String) return Integer is
      Input : Integer;
   begin
      Put(Prompt);
      Get(Input);
      return Input;
   exception
      when others =>
         Put_Line("Entrée invalide. Veuillez réessayer.");
         return Get_User_Input(Prompt);
   end Get_User_Input;

begin
   loop
      Show_Menu;
      declare
         Choice : Integer := Get_User_Input("Votre choix : ");
      begin
         case Choice is
            when 1 =>
               if not Is_Null(Tree) then
                  Put_Line("L'arbre a déjà une racine !");
               else
                  declare
                     Value : Integer := Get_User_Input("Entrez la valeur de la racine : ");
                     Id    : Natural := 1;
                  begin
                     Initialise(Tree, Id, Value);
                     Put_Line("Racine initialisée.");
                  end;
               end if;

            when 2 =>
               if Is_Null(Tree) then
                  Put_Line("L'arbre n'a pas de racine. Initialisez-le d'abord.");
               else
                  declare
                     Value : Integer := Get_User_Input("Entrez la valeur du nœud gauche : ");
                     Id    : Natural := 2;
                  begin
                     Add_Left(Tree, Id, Value);
                     Put_Line("Nœud gauche ajouté.");
                  end;
               end if;

            when 3 =>
               if Is_Null(Tree) then
                  Put_Line("L'arbre n'a pas de racine. Initialisez-le d'abord.");
               else
                  declare
                     Value : Integer := Get_User_Input("Entrez la valeur du nœud droit : ");
                     Id    : Natural := 3;
                  begin
                     Add_Right(Tree, Id, Value);
                     Put_Line("Nœud droit ajouté.");
                  end;
               end if;

            when 4 =>
               if Is_Null(Tree) then
                  Put_Line("L'arbre n'a pas de racine. Initialisez-le d'abord.");
               elsif Is_Null(Get_Left(Tree)) then
                  Put_Line("Le nœud gauche est déjà vide.");
               else
                  Remove_Left(Tree);
                  Put_Line("Nœud gauche supprimé.");
               end if;

            when 5 =>
               if Is_Null(Tree) then
                  Put_Line("L'arbre n'a pas de racine. Initialisez-le d'abord.");
               elsif Is_Null(Get_Right(Tree)) then
                  Put_Line("Le nœud droit est déjà vide.");
               else
                  Remove_Right(Tree);
                  Put_Line("Nœud droit supprimé.");
               end if;

            when 6 =>
               if Is_Null(Tree) then
                  Put_Line("L'arbre est vide.");
               else
                  -- Put_Line("Valeur de la racine : " & Integer'Image(Get_Value(Tree)));
                  Display (Tree);
               end if;

            when 7 =>
               Put_Line("Au revoir !");
               exit;

            when others =>
               Put_Line("Choix invalide. Veuillez réessayer.");
         end case;
      end;
   end loop;
end Main;
