import java.util.ArrayList;
import java.util.Iterator;

public class H7 {
	
	ArrayList<Player> playerList = new ArrayList<>(); //list of all the current playing players
	ArrayList<Chair> chairList = new ArrayList<>(); //list of all the chairs in the game
	ArrayList<String> notPlaying = new ArrayList<>(); //list of the names of players no longer playing
	int currentPlayers; //keep track of the current players in the game
	int doneLooking = 0; //keep track of the number of players that have finished looking for a chair 
	int music = 1; //0 for OFF, 1 for ON
	int round = 1; //current round
	boolean mainThreadDone = false; //to know when the main thread is done performing its operations. 
	boolean release = true; //makes the main thread wait until all players are finished playing
	
	/*
	 * This will turn off the music and allow
	 * the threads/players to then grab chairs. 
	 * MAIN THREAD ONLY
	 */
	public void turnOffMusic() { 
		System.out.println("music off");
		doneLooking = 0; //reset for a new round
		music = 0;
	}
	
	public void play() {
		while(chairList.size() != 0) { //while there are chairs in the game, keep playing.
			while(music == 1) { 
					//spin lock, music is still on
			}
			mainThreadDone = false; //reset
			if(notPlaying.contains(Thread.currentThread().getName()) == false) { //players that are still playing
				grabChairs();
				doneLooking += 1; //a player has finished looking for a chair
			}
			while (!mainThreadDone) { 
				//spin lock, players will have to wait for other players to finish looking and for the mainthread to complete its operations
			}
		}
	}
	
	/*
	 * This will allow a player to grab a chair,
	 * if a player does not grab one, then kick him out. 
	 */
	public void grabChairs() {
		for (Chair c: chairList) {
			synchronized(this) { //this section is for grabbing and sitting in a chair, one at a time
				if(c.occupied == false) {
					System.out.println("HERE");
					c.occupied = true;
					System.out.println(Thread.currentThread().getName() + " sat in C" + c.chairNum);
					setSitting(Thread.currentThread().getName());
					break;
				}
			}
		}
	}
	
	/*
	 * Sets the sitting variable to true
	 * for the player that sits on a chair
	 */
	public void setSitting(String pname) {
		for (Player p: playerList) {
			if (p.playerName.equals(pname)) {
				p.sitting = true;
			}
		}
	}
	
	/*
	 * Gets the player who did not find a chair
	 * and removes them from the list. 
	 * MAIN THREAD ONLY
	 */
	public void findLoser() {
		Player loser = null;
		
		for (Player p: playerList) {
			if (p.sitting == false) {
				loser = p;
				break;
			}
		}
	
		playerList.remove(loser);
		notPlaying.add(loser.playerName); //this player is no longer playing. 
		currentPlayers -= 1; //reduce player count
		System.out.println(loser.playerName + " lost\n");
	}
	
	/*
	 * Resets the sitting variable for all players
	 * since no one is sitting on a chair anymore at 
	 * the start of a new round
	 * MAIN THREAD ONLY
	 */
	public void resetSitting() {
		for (Player p: playerList) {
			p.sitting = false;
		}
	}

	
	/*
	 * Resets all the chairs to unoccupied
	 * MAIN THREAD ONLY
	 */
	public void chairReset() {
		for (Chair c: chairList) {
			c.occupied = false; 
		}
	}
	
	public static void main(String[] args) {
		//int n = Integer.parseInt(args[0]);
		int n = 10;
		H7 game = new H7();
		
		System.out.println("BEGIN " + n + " players\n");
		game.currentPlayers = n;
		
		//Creates our players and chair objects
		for(int i = 0; i < n; i++) {
			if(i < n-1) {
				game.playerList.add(new Player(i+1, game));
				game.chairList.add(new Chair(i+1));
				
			}
			else {
				game.playerList.add(new Player(i+1, game));
			}
		}
		
		//release the players
		for (final Iterator<Player> it = game.playerList.iterator(); it.hasNext();) {
			  it.next().start();
		}
		
		while(game.round < n) {
			System.out.println("Round " + game.round);
			game.turnOffMusic(); //release the players
			while(game.doneLooking != game.currentPlayers) {
				//spin lock, players still looking for chair
			}
			game.round += 1;
			game.chairList.remove(game.chairList.size() - 1); //removes the last numbered chair 
			game.chairReset(); //reset the chairs
			game.findLoser();
			game.resetSitting();  //no players is sitting on a chair anymore.
			game.music = 1; //turn the music back on so players can wait 
			game.mainThreadDone = true; //main thread is done with its actions.
		}
		
		System.out.println(game.playerList.get(0).playerName + " wins!");
		System.out.println("END");

	}

}
