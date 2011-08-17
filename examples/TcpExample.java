import org.hurricane.driver.Gateway;
import org.hurricane.driver.SocketWrapper;
import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.Tuple;
import org.hurricane.driver.datatypes.Nil;

public class TcpExample {
    public static void main(String[] args) throws Exception {
        Gateway gateway = new Gateway("localhost", 3307);

        while (true) {
            gateway.send(
                new Tuple()
                    .append(new Atom("request"))
                    .append(new Atom("time_server"))
                    .append(new Atom("time_message"))
                    .append(new Nil())
            );
            System.out.println(gateway.recv());
        }
    }
}
