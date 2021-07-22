package util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

final class LineCount {
    public static void main( String[] args ) throws IOException {
        for ( String arg : args ) {
            countLines( arg );
        }
    }

    private static void countLines( String name ) throws IOException {
        var file = new File( name );
        var count = 0;
        for ( var line : Files.readAllLines( file.toPath() ) ) {
            line = line.trim();
            var isComment =
                    /// C-like languages
                    line.startsWith( "//" ) ||
                            line.startsWith( "*" ) ||
                            line.startsWith( "/*" ) ||
                            // Python
                            line.startsWith( "#" ) ||
                            // Haskell, Lua
                            line.startsWith( "--" ) ||
                            line.startsWith( "{--" ) ||
                            // Lisp (TODO do not count long string comments)
                            line.startsWith( ";" );

            var isImport = line.startsWith( "import " ) ||
                    line.startsWith( "use " ) ||
                    line.startsWith( "from " );

            if ( !isComment && !isImport && !line.isEmpty() ) count++;
        }

        System.out.println( file + ": " + count );
    }
}
