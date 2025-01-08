package body Arbre_Bin is

   -- Ajouter une racine à l'arbre
   procedure Add_Root(Tree : in out T_Arbre_Bin; Value : in Element_Type) is
   begin
      if Tree.Root = null then
         Tree.Root := new T_Node'(Id => Natural'First, Value => Value, Right => null, Left => null);
      else
         raise Program_Error with "Root already exists.";
      end if;
   end Add_Root;

   -- Ajouter un nœud à gauche
   procedure Add_Left(Tree : in out T_Arbre_Bin; Parent_Value : in Element_Type; Value : in Element_Type) is
      Parent_Node : T_Noeud_Bin;
   begin
      -- Trouver le nœud parent (par exemple, on pourrait faire une recherche ici)
      -- À la place, on suppose que l'on ajoute simplement à la racine pour cet exemple
      if Tree.Root /= null then
         Noeud_Bin.Initialise(Parent_Node, Parent_Value);
         Noeud_Bin.Add_Left(Parent_Node, Value);
      else
         raise Program_Error with "Tree is empty.";
      end if;
   end Add_Left;

   -- Ajouter un nœud à droite
   procedure Add_Right(Tree : in out T_Arbre_Bin; Parent_Value : in Element_Type; Value : in Element_Type) is
      Parent_Node : T_Noeud_Bin;
   begin
      -- Trouver le nœud parent (similaire à Add_Left)
      if Tree.Root /= null then
         Noeud_Bin.Initialise(Parent_Node, Parent_Value);
         Noeud_Bin.Add_Right(Parent_Node, Value);
      else
         raise Program_Error with "Tree is empty.";
      end if;
   end Add_Right;

   -- Afficher l'arbre
   procedure Display(Tree : in T_Arbre_Bin) is
   begin
      if Tree.Root /= null then
         Noeud_Bin.Display(Tree.Root.all);
      else
         raise Program_Error with "Tree is empty.";
      end if;
   end Display;

   -- Vider l'arbre
   procedure Clear(Tree : in out T_Arbre_Bin) is
   begin
      if Tree.Root /= null then
         Tree.Root := null;
      end if;
   end Clear;

end Arbre_Bin;
