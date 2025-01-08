generic
   type Element_Type is private; -- Type générique pour le contenu du nœud
package Noeud_Bin is

   -- Type représentant un nœud binaire
   type T_Noeud_Bin is private;

   -- Initialiser un nœud
   procedure Initialise(Node : in out T_Noeud_Bin; Value : in Element_Type);

   -- Ajouter un successeur à droite
   procedure Add_Right(Node : in out T_Noeud_Bin; Value : in Element_Type);

   -- Ajouter un successeur à gauche
   procedure Add_Left(Node : in out T_Noeud_Bin; Value : in Element_Type);

   -- Supprimer le successeur droit
   procedure Remove_Right(Node : in out T_Noeud_Bin);

   -- Supprimer le successeur gauche
   procedure Remove_Left(Node : in out T_Noeud_Bin);

   -- Obtenir le successeur droit
   function Get_Right(Node : in T_Noeud_Bin) return T_Noeud_Bin;

   -- Obtenir le successeur gauche
   function Get_Left(Node : in T_Noeud_Bin) return T_Noeud_Bin;

   -- Obtenir l'identifiant (ou autre valeur liée au nœud)
   function Get_Id(Node : in T_Noeud_Bin) return Natural;

   -- Obtenir la valeur contenue dans le nœud
   function Get_Value(Node : in T_Noeud_Bin) return Element_Type;

   -- Afficher un nœud (procédure générique pour adapter l'affichage)
   generic
      with procedure Display_Element(Value : in Element_Type); -- Procédure pour afficher un élément
   procedure Display(Node : in T_Noeud_Bin);

private

   type T_Node is
      record
         Id     : Natural;
         Value  : Element_Type; -- Contenu du nœud
         Right  : T_Noeud_Bin_Access;  -- Successeur droit
         Left   : T_Noeud_Bin_Access;  -- Successeur gauche
      end record;

   type T_Noeud_Bin_Access is access all T_Noeud_Bin;

end Noeud_Bin;
