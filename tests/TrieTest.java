import org.junit.jupiter.api.Test;

import java.util.Set;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class TrieTest {

    public static final Item MIR = new Item( "mir" );
    public static final Item TOR = new Item( "Tor", "tor" );
    public static final Item MIX = new Item( "Mix", "mix" );

    public static final Item W_0 = new Item( "0" );
    public static final Item W_1 = new Item( "1" );

    @Test
    void canFindWordsUsingTrie() {
        var trie = new Trie();
        trie.put( MIR );
        trie.put( TOR );
        trie.put( MIX );

        Item phone_562 = new Item( "562" );
        Item phone_5621 = new Item( "5621" );
        Item phone_562482 = new Item( "562482" );
        Item phone_5620482 = new Item( "5620482" );

        assertEquals( Set.of( solution( phone_562, MIR ), solution( phone_562, MIX ) ),
                trie.get( phone_562 ) );
        assertEquals( Set.of( solution( phone_5621, MIR, W_1 ), solution( phone_5621, MIX, W_1 ) ),
                trie.get( phone_5621 ) );
        assertEquals( Set.of( solution( phone_562482, MIR, TOR ), solution( phone_562482, MIX, TOR ) ),
                trie.get( phone_562482 ) );
        assertEquals( Set.of( solution( phone_5620482, MIR, W_0, TOR ), solution( phone_5620482, MIX, W_0, TOR ) ),
                trie.get( phone_5620482 ) );
    }

    private static Item solution( Item phone, Item... words ) {
        return new Item( phone.original, Stream.of( words ).map( w -> w.original ).collect( joining( " " ) ) );
    }
}
