with Noeud_Bin;

generic
   -- Le type du contenu du nœud (éléments stockés dans l'arbre)
   type Element_Type is private;
package Arbre_Bin is

   -- Type représentant un arbre binaire (racine de l'arbre)
   type T_Arbre_Bin is private;

   -- Ajouter une racine à l'arbre
   procedure Add_Root(Tree : in out T_Arbre_Bin; Value : in Element_Type);

   -- Ajouter un nœud à gauche
   procedure Add_Left(Tree : in out T_Arbre_Bin; Parent_Value : in Element_Type; Value : in Element_Type);

   -- Ajouter un nœud à droite
   procedure Add_Right(Tree : in out T_Arbre_Bin; Parent_Value : in Element_Type; Value : in Element_Type);

   -- Afficher les éléments de l'arbre (parcours préfixé)
   generic
      with procedure Display_Element(Value : in Element_Type); -- Procédure pour afficher un élément
   procedure Display(Tree : in T_Arbre_Bin);

   -- Vider l'arbre
   procedure Clear(Tree : in out T_Arbre_Bin);

private

   type Node_Access is access all Noeud_Bin.T_Noeud_Bin;

   type T_Arbre_Bin is record
      Root : Node_Access := null; -- Pointeur vers le nœud racine
   end record;

end Arbre_Bin;
