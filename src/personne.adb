with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body Personne is

   -- Implémentation de la procédure Initialiser
   procedure Initialise
     (P              : out T_Personne;
      Prenom         : in String;
      Nom            : in String;
      Sexe           : in String;
      Date_Naissance : in Ada.Calendar.Time;
      Lieu_Naissance : in String;
      Date_Deces     : access Ada.Calendar.Time := null) is
   begin
      P.Prenom := new String'(Prenom);
      P.Nom := new String'(Nom);
      P.Sexe := new String'(Sexe);
      P.Date_De_Naissance := Date_Naissance;
      P.Lieu_De_Naissance := new String'(Lieu_Naissance);
      P.Date_De_Deces := Date_Deces;
   end Initialise;

   -- Implémentation de la procédure Afficher
   procedure Display(P : in T_Personne) is
   begin
      Put_Line("Prenom: " & P.Prenom.all);
      Put_Line("Nom: " & P.Nom.all);
      Put_Line("Sexe: " & P.Sexe.all);
      Put_Line("Date de Naissance: " & Image(P.Date_De_Naissance));
      Put_Line("Lieu de Naissance: " & P.Lieu_De_Naissance.all);
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
         & Image(P.Date_De_Naissance) & " | "
         & (if P.Date_De_Deces /= null then Image(P.Date_De_Deces.all) & " |" else "");

   end To_String;

end Personne;