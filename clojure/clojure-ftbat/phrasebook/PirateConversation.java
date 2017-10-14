import pirate_phrases.*;

public class PirateConversation
{
    public static void main(String[] args)
    {
        Greetings greettings = new Greetings();
        greettings.hello();

        Farewells farewells = new Farewells();
        farewells.goodbye();
    }
}
