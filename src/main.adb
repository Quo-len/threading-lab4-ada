with Ada.Text_IO;     use Ada.Text_IO;
with GNAT.Semaphores; use GNAT.Semaphores;

procedure Main is
   task type PhilosopherRoom is
      entry Start (Id : Integer);
   end PhilosopherRoom;

   Forks : array (1 .. 5) of Counting_Semaphore (1, Default_Ceiling);

   -- room

   Room : Counting_Semaphore (4, Default_Ceiling);

   task body PhilosopherRoom is
      Id                          : Integer;
      Id_Left_Fork, Id_Right_Fork : Integer;
   begin
      accept Start (Id : in Integer) do
         PhilosopherRoom.Id := Id;
      end Start;
      Id_Left_Fork  := Id;
      Id_Right_Fork := Id rem 5 + 1;

      for I in 1 .. 10 loop
         Put_Line ("Philosopher " & Id'Img & " is hungry " & I'Img & " time");

         Room.Seize;
         Forks (Id_Left_Fork).Seize;
         Forks (Id_Right_Fork).Seize;

         Put_Line ("Philosopher " & Id'Img & " is eating" & I'Img & " time");

         Forks (Id_Right_Fork).Release;
         Forks (Id_Left_Fork).Release;
         Room.Release;

         Put_Line ("Philosopher " & Id'Img & " finished eating");
      end loop;
   end PhilosopherRoom;

   -- asymmetric and order

   task type Philosopher is
      entry StartOrder (Id : Integer);
      entry StartAsymmetric (Id : Integer);
   end Philosopher;

   task body Philosopher is
      Id                          : Integer;
      Id_Left_Fork, Id_Right_Fork : Integer;
   begin
      select
         accept StartOrder (Id : in Integer) do
            Philosopher.Id := Id;
            if Id < (Id rem 5 + 1) then
               Id_Left_Fork  := Id;
               Id_Right_Fork := Id rem 5 + 1;
            else
               Id_Left_Fork  := Id rem 5 + 1;
               Id_Right_Fork := Id;
            end if;
         end StartOrder;
      or
         accept StartAsymmetric (Id : in Integer) do
            Philosopher.Id := Id;
            if Id rem 2 = 0 then
               Id_Left_Fork  := Id;
               Id_Right_Fork := Id rem 5 + 1;
            else
               Id_Left_Fork  := Id rem 5 + 1;
               Id_Right_Fork := Id;
            end if;
         end StartAsymmetric;
      end select;
      for I in 1 .. 10 loop
         Put_Line ("Philosopher " & Id'Img & " is hungry " & I'Img & " time");

         Forks (Id_Left_Fork).Seize;
         Forks (Id_Right_Fork).Seize;

         Put_Line ("Philosopher " & Id'Img & " is eating" & I'Img & " time");

         Forks (Id_Right_Fork).Release;
         Forks (Id_Left_Fork).Release;

         Put_Line ("Philosopher " & Id'Img & " finished eating");
      end loop;
   end Philosopher;

   -- arbitrator --

   protected type MySemaphore (Start_Count : Integer := 1) is

      entry Secure;
      procedure Release;
      function Count return Integer;
   private
      Current_Count : Integer := Start_Count;
   end MySemaphore;

   protected body MySemaphore is

      entry Secure when Current_Count > 0 is
      begin
         Current_Count := Current_Count - 1;
      end Secure;

      procedure Release is
      begin
         Current_Count := Current_Count + 1;
      end Release;

      function Count return Integer is
      begin
         return Current_Count;
      end Count;

   end MySemaphore;

   ForksArbitrator : array (1 .. 5) of MySemaphore (1);
   Arbitrator      : Counting_Semaphore (2, Default_Ceiling);

   task type PhilosopherArbitrator is
      entry Start (Id : Integer);
   end PhilosopherArbitrator;

   task body PhilosopherArbitrator is
      Id                          : Integer;
      Id_Left_Fork, Id_Right_Fork : Integer;
   begin
      accept Start (Id : in Integer) do
         PhilosopherArbitrator.Id := Id;
      end Start;
      Id_Left_Fork  := Id;
      Id_Right_Fork := Id rem 5 + 1;

      for I in 1 .. 10 loop
         Put_Line ("Philosopher " & Id'Img & " is hungry " & I'Img & " time");
         while True loop
            Arbitrator.Seize;
            if ForksArbitrator (Id_Left_Fork).Count > 0 and
              ForksArbitrator (Id_Right_Fork).Count > 0
            then
               ForksArbitrator (Id_Left_Fork).Secure;
               ForksArbitrator (Id_Right_Fork).Secure;
               exit;
            else
               Arbitrator.Release;

            end if;
         end loop;
         Arbitrator.Release;
         Put_Line ("Philosopher " & Id'Img & " is eating" & I'Img & " time");

         ForksArbitrator (Id_Left_Fork).Release;
         ForksArbitrator (Id_Right_Fork).Release;

         Put_Line ("Philosopher " & Id'Img & " finished eating");
      end loop;
   end PhilosopherArbitrator;

   Philosophers : array (1 .. 5) of Philosopher;
begin
   for I in Philosophers'Range loop
      Philosophers (I).StartOrder (I);
   end loop;

end Main;
