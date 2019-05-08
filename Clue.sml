(* AUTHORS: Eric Michaelson, James White *)

(* DATATYPES *)

(*A datatype to represent each possible card in the game.*)
datatype card = MrGreen | ProfessorPlum | ColonelMustard | MsScarlet | MrsPeacock | MrsWhite | Knife | Candlestick | Revolver | LeadPipe | Rope | Wrench | Ballroom | BilliardRoom | Conservatory | DiningRoom | Hall | Kitchen | Library | Lounge | Study |  Nothing | Default;

(*A tuple that holds the cards given to a player at the beginning of the game*)
datatype player = Player of card * card * card;

(* LISTS *)

(*Lists to distinguish between different types of cards, useful for assertTrio()*)
val personL = [MrGreen , ProfessorPlum , ColonelMustard , MsScarlet , MrsPeacock , MrsWhite];
val weaponL = [Knife , Candlestick , Revolver , LeadPipe , Rope , Wrench];
val roomL = [Ballroom , BilliardRoom , Conservatory , DiningRoom , Hall , Kitchen , Library , Lounge , Study];

(* INSTANCE VARIABLES *)

(*List that stands for the cards  to each of the six players*)
val allPlayers = ref [[Default]];

(*List representing the players left in the game*)
val activePlayers = ref [[Default]];

(*Tuples of players and passwords, used to encode each player’s turn using the suggest() and accuse() functions. A pair is included for type inference.*)
val passwords = ref [(0,[Default])];

(*Variable for the current player, the only one allowed to make the next turn*)
val currentPlayer = ref [Default];

(*Variable to represent the middle of the board, the cards to discover*)
val conf = ref [Default];

(* HELPER FUNCTIONS *)

(*A function to test whether a list contains a value. Used in sameList*)
fun contains(x,[]) = false
| contains(x,y::rest) = if x = y then true else contains(x,rest);

(*Asserts that two lists are the same.*)
fun sameList([],z) = true
| sameList(x::rest,z) = if contains(x,z) then sameList(rest,z) else false;

(*A function to remove an element from a list*)
fun delete(x,(first::rest)) =
        if first = x 
        then rest 
        else first::delete(x,rest);

(*Converts a password into a player’s values, if there is a match.*)
fun usePassword(a, []) = []
      | usePassword(a,(k,v)::rest) = if a = k
    then v
    else usePassword(a, rest);

(*Gets a password when given the valid player list associated with that password*)
fun getPassword(a, []) = 0
      | getPassword(a,(k, v)::rest) = if sameList(a,v)
    then k
    else getPassword(a, rest);

(*Retrieves the player after the player inputted.*)
fun incPlayer(player,p::n::rest) =
    if sameList(player,p)
    then n
    else incPlayer(player,n::rest@[p]);

(*Makes a formal assertion to ensure that a guess is a proper person, weapon list.*)
fun assertTrio([first,second,third]) =
        contains(first,personL)
        andalso contains(second,weaponL)
        andalso contains(third,roomL)
|   assertTrio(a) = false;

(* Makes the next player in activePlayers the current player*)
fun nextTurn() =
    let
        fun helper(player,[]) = ()
        |   helper(player,p::n::rest) = 
                if sameList(player,p)
                then currentPlayer := n
                else helper(player,n::rest@[p])
    in
        helper(!currentPlayer,!activePlayers)
    end;

(*Uses assertTrio and checks if password matches currentPlayer*)
fun assertPlayer(pass,guess) = 
    assertTrio(guess) andalso 
    sameList(usePassword(pass,!passwords),!currentPlayer);

(* FUNCTIONS *)

(*Function that takes a password and suggestion, then simulates motion through the players, returning the next found card if it exists.*)
fun suggest(pass,suggestion) =
    if assertPlayer(pass,suggestion)
    then
        let
            fun helper([],nextplayer) =
                helper(suggestion,incPlayer(nextplayer,!allPlayers))
                  | helper(a::restsuggest,nextplayer) =
                if contains(a,nextplayer)
                then (
                    print("Another player has shown you a card!\n");
                    a
                )
                else helper(restsuggest,nextplayer);
        in (
            nextTurn();
            helper(suggestion, incPlayer(usePassword(pass,!passwords),!allPlayers))
        )
        end
    else (
        print("Bad guess.\n");
        Nothing
    )

(*Used to guess if a Trio is identical to the cards in confidential.*)
fun accuse(pass,guess) =
    if assertPlayer(pass,guess)
    then 
        if sameList(guess,!conf)
            then print("Correct! You win!\n")
            else
            let
                val toRemove = !currentPlayer
            in (
                nextTurn();
                activePlayers := delete(toRemove,!activePlayers);
                print("Incorrect . . . " ^ Int.toString(pass) ^ " is no longer in the game.\n")
            )
            end
    else print("Bad guess or wrong player.\n");

(* CONSTRUCTION *)

(*Instance-specific initialization of password pairs, confidential, and currentplayer. It could be imagined that the cards were dealt, and an arbitrator
inputted the variable conf, currentPlayer, and all the password pairs.*)

print("\nVariable assignment:\n");
conf := [MrGreen,Knife,Ballroom];

currentPlayer := [ProfessorPlum,ColonelMustard,Candlestick];

allPlayers :=
[[ProfessorPlum,ColonelMustard,Candlestick],
[MsScarlet,Kitchen,Revolver],
[MrsWhite,LeadPipe,DiningRoom],
[Rope,Library,MrsPeacock],
[Hall,Conservatory,Study],
[Wrench,Ballroom,BilliardRoom]];

activePlayers := !allPlayers;

passwords :=
[(111,[ProfessorPlum,ColonelMustard,Candlestick]),
(222,[MsScarlet,Kitchen,Revolver]),
(333,[MrsWhite,LeadPipe,DiningRoom]),
(444,[Rope,Library,MrsPeacock]),
(555,[Hall,Conservatory,Study]),
(666,[Wrench,Ballroom,BilliardRoom])];

(* SIMULATION *)

(*Example of a handful of turns occuring in the game:
1. Player 1 suggests successfully.
2. Player 1 suggests out of turn.
3. Player 2 suggests successfully.
4. Player 3 suggests successfully.
5. Player 4 suggests a bad trio.
6a.Player 4 accuses an incorrect trio.
    6b. Player 4 is kicked from the game in the background,
        but kept in allPlayers to accommadate showing players
        cards for the remainder of the game.
7. Player 5 suggests successfully.
8. Player 6 accuses successfully.
*)

print("\nPlayer 6 shows player 1 a card:\n\n");
suggest(111,[ProfessorPlum,Wrench,Ballroom]);

print("\nWrong player example:\n\n");
suggest(111,[ProfessorPlum,Wrench,Ballroom]);

print("\nPlayer 5 shows player 2 a card:\n\n");
suggest(222,[MsScarlet,Knife,Study]);

print("\nPlayer 5 shows player 3 a card:\n\n");
suggest(333,[MsScarlet,Knife,Study]);

print("\nBad trio example:\n\n");
suggest(444,[Revolver,Wrench,ProfessorPlum]);

print("\nWrong Accusation:\n\n");
accuse(444,[ProfessorPlum,Wrench,Ballroom]);

print("\nNew player list, without 444:\n\n");
!activePlayers;

print("\nPlayer 4 shows player 5 a card:\n\n");
suggest(555,[MrsPeacock,Rope,Study]);

print("\nCorrect Accusation by player 6:\n\n");
accuse(666,[MrGreen,Knife,Ballroom]);

(* EOP *)