import org.hurricane.driver.Gateway;
import org.hurricane.driver.SocketWrapper;
import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.Tuple;
import org.hurricane.driver.datatypes.Nil;

public class TcpExample {
    public static void main(String[] args) throws Exception {
        Gateway gateway = new Gateway("localhost", 3307);

        Tuple sendMsg;
        while (true) {
            sendMsg = new Tuple();
            sendMsg.elements().add(new Atom("request"));
            sendMsg.elements().add(new Atom("time_server"));
            sendMsg.elements().add(new Atom("time_message"));
            sendMsg.elements().add(new Nil());
            gateway.send(sendMsg);

            System.out.println(gateway.recv());
        }
    }
}
