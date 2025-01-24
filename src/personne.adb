with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Unchecked_Deallocation;

package body Personne is

   procedure Free_String is new Ada.Unchecked_Deallocation(String, String_Access);
   procedure Free_Time is new Ada.Unchecked_Deallocation(Ada.Calendar.Time, Time_Access);

   -- Implémentation de la procédure Initialiser
   procedure Initialise
     (P              : out T_Personne;
      Prenom         : in String;
      Nom            : in String;
      Sexe           : in String;
      Date_Naissance : Time_Access;
      Lieu_Naissance : in String;
      Date_Deces     : Time_Access := null) is
   begin
      P.Prenom := new String'(Prenom);
      P.Nom := new String'(Nom);
      P.Sexe := new String'(Sexe);
      P.Date_De_Naissance := new Ada.Calendar.Time'(Date_Naissance.all);
      P.Lieu_De_Naissance := new String'(Lieu_Naissance);
      
      if Date_Deces /= null then
         P.Date_De_Deces := new Ada.Calendar.Time'(Date_Deces.all);
      else
         P.Date_De_Deces := null;
      end if;

   end Initialise;


   function Get_Name(P : in T_Personne) return access String is
   begin 
      if (P.Nom = null) then  
         return new String'("");
      end if;
      return P.Nom;
   end Get_Name;

   function Get_First_Name(P : in T_Personne) return access String is
   begin 
      if (P.Prenom = null) then  
         return new String'("");
      end if;
      return P.Prenom;
   end Get_First_Name;

   function Get_Gender(P : in T_Personne) return access String is 
   begin 
      if (P.Sexe = null) then  
         return new String'("");
      end if;
      return P.Sexe;
   end Get_Gender;

   -- Implémentation de la procédure Afficher
   procedure Display(P : in T_Personne) is
   begin
      if (P.Prenom /= null and P.Nom /= null) then 
         Put_Line("Prenom: " & P.Prenom.all);
         Put_Line("Nom: " & P.Nom.all);
         Put_Line("Sexe: " & P.Sexe.all);
         Put_Line("Date de Naissance: " & Image(P.Date_De_Naissance.all));
         Put_Line("Lieu de Naissance: " & P.Lieu_De_Naissance.all);
      end if;
      if P.Date_De_Deces /= null then
         Put_Line("Date de Décès: " & Image(P.Date_De_Deces.all));
      end if;
   end Display;

   function To_String(P: in T_Personne) return String is
   begin

      return P.Prenom.all & " | "
         & P.Nom.all & " | "
         & P.Sexe.all & " | "
         & P.Lieu_De_Naissance.all & " | "
         & Image(P.Date_De_Naissance.all) & " | "
         & (if P.Date_De_Deces /= null then Image(P.Date_De_Deces.all) & " |" else "");

   end To_String;

   procedure Free_Element(P : in out T_Personne) is
   begin
      Free_String(P.Prenom);
      Free_String(P.Nom);
      Free_String(P.Sexe);
      Free_String(P.Lieu_De_Naissance);
      Free_Time(P.Date_De_Naissance);
      if P.Date_De_Deces /= null then
         Free_Time(P.Date_De_Deces);
      end if;
end Free_Element;


end Personne;