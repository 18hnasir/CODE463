import javax.swing.plaf.synth.SynthOptionPaneUI;

public class Player extends Thread {
	
	String playerName; //player number
	boolean sitting; //is this player sitting on a chair 
	H7 game;
	
	public Player(int playerNum, H7 game) {
		this.playerName = "P" + playerNum;
		setName(this.playerName); //Ex. P1
		this.sitting = false; //not sitting on chair
		this.game = game;
	}
	
	public void run() {
		game.play();
	}

}
