with arbre_bin;
with noeud_bin;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   -- Déclare une variable de type Arbre_Bin
   Tree : Arbre_Bin.T_Arbre_Bin;

   -- Déclare des variables pour les nœuds
   Node1, Node2 : Noeud_Bin.T_Noeud_Bin;
begin
   -- Initialiser l'arbre avec un nœud racine
   Put_Line("Initialisation de l'arbre avec une racine.");
   Arbre_Bin.Add_Root(Tree, 10);  -- Ajoute une racine avec la valeur 10

   -- Ajouter un successeur à gauche de la racine
   Put_Line("Ajout d'un successeur à gauche de la racine.");
   Arbre_Bin.Add_Left(Tree, 10, 5);  -- Ajoute à gauche de la racine (10)

   -- Ajouter un successeur à droite de la racine
   Put_Line("Ajout d'un successeur à droite de la racine.");
   Arbre_Bin.Add_Right(Tree, 10, 15);  -- Ajoute à droite de la racine (10)

   -- Afficher l'arbre
   Put_Line("Affichage de l'arbre.");
   Arbre_Bin.Display(Tree);

   -- Tester l'ajout de nœuds à gauche et à droite
   Put_Line("Ajout d'un nœud à gauche de 5.");
   Noeud_Bin.Initialise(Node1, 3);
   Noeud_Bin.Add_Left(Node1, 1);  -- Ajoute un nœud avec la valeur 1 à gauche de Node1

   Put_Line("Affichage du nœud Node1.");
   Noeud_Bin.Display(Node1);  -- Affiche le nœud Node1

   -- Supprimer un nœud à gauche
   Put_Line("Suppression du nœud à gauche de Node1.");
   Noeud_Bin.Remove_Left(Node1);  -- Supprime le nœud gauche de Node1

   -- Afficher le nœud après suppression
   Put_Line("Affichage du nœud Node1 après suppression.");
   Noeud_Bin.Display(Node1);

   -- Tester la suppression d'un nœud
   Put_Line("Suppression du nœud à droite de la racine.");
   Arbre_Bin.Remove_Right(Tree);  -- Supprime le nœud droit de la racine

   -- Afficher l'arbre après suppression
   Put_Line("Affichage de l'arbre après suppression du nœud droit.");
   Arbre_Bin.Display(Tree);
   
end Main;
