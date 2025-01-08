package body Noeud_Bin is

   -- Initialiser un nœud avec une valeur
   procedure Initialise(Node : in out T_Noeud_Bin; Value : in Element_Type) is
   begin
      Node := new T_Noeud_Bin'(Id => Natural'First, Value => Value, Right => null, Left => null);
   end Initialise;

   -- Ajouter un successeur à droite
   procedure Add_Right(Node : in out T_Noeud_Bin; Value : in Element_Type) is
   begin
      if Node.Right = null then
         Node.Right := new T_Node'(Id => Natural'First, Value => Value, Right => null, Left => null);
      else
         raise Program_Error with "Right node already exists.";
      end if;
   end Add_Right;

   -- Ajouter un successeur à gauche
   procedure Add_Left(Node : in out T_Noeud_Bin; Value : in Element_Type) is
   begin
      if Node.Left = null then
         Node.Left := new T_Node'(Id => Natural'First, Value => Value, Right => null, Left => null);
      else
         raise Program_Error with "Left node already exists.";
      end if;
   end Add_Left;

   -- Supprimer le successeur droit
   procedure Remove_Right(Node : in out T_Noeud_Bin) is
   begin
      if Node.Right /= null then
         Node.Right := null;
      else
         raise Program_Error with "No right node to remove.";
      end if;
   end Remove_Right;

   -- Supprimer le successeur gauche
   procedure Remove_Left(Node : in out T_Noeud_Bin) is
   begin
      if Node.Left /= null then
         Node.Left := null;
      else
         raise Program_Error with "No left node to remove.";
      end if;
   end Remove_Left;

   -- Obtenir le successeur droit
   function Get_Right(Node : in T_Noeud_Bin) return T_Noeud_Bin is
   begin
      if Node.Right /= null then
         return Node.Right.all;
      else
         raise Program_Error with "No right node.";
      end if;
   end Get_Right;

   -- Obtenir le successeur gauche
   function Get_Left(Node : in T_Noeud_Bin) return T_Noeud_Bin is
   begin
      if Node.Left /= null then
         return Node.Left.all;
      else
         raise Program_Error with "No left node.";
      end if;
   end Get_Left;

   -- Obtenir l'identifiant du nœud
   function Get_Id(Node : in T_Noeud_Bin) return Natural is
   begin
      return Node.Id;
   end Get_Id;

   -- Obtenir la valeur contenue dans le nœud
   function Get_Value(Node : in T_Noeud_Bin) return Element_Type is
   begin
      return Node.Value;
   end Get_Value;

   -- Afficher un nœud
   procedure Display(Node : in T_Noeud_Bin) is
   begin
      Display_Element(Node.Value); -- Appel à la procédure générique pour afficher l'élément
   end Display;

end Noeud_Bin;
