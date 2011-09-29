import org.hurricane.driver.Gateway;
import org.hurricane.driver.SocketWrapper;
import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.Tuple;
import org.hurricane.driver.datatypes.Nil;
import java.util.Date;

public class TcpServerExample {
    public static void main(String[] args) throws Exception {
        Gateway gateway = new Gateway("localhost", 3307);
        Tuple regMsg = new Tuple();
        regMsg.elements().add(new Atom("register_with_group"));
        regMsg.elements().add(new Atom("time_server"));
        gateway.send(regMsg);

        while (true) {
            Tuple recvMsg = (Tuple) gateway.recv();
            Atom type = (Atom) recvMsg.elements().get(0);
            if (type.getName().equals("request")) {
                Tuple sendMsg = new Tuple();
                sendMsg.elements().add(new Atom("response"));
                sendMsg.elements().add(recvMsg.elements().get(1));
                sendMsg.elements().add(recvMsg.elements().get(2));
                sendMsg.elements().add(new Date().toString());
                gateway.send(sendMsg);
            }
        }
    }
}
