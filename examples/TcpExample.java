import org.hurricane.Gateway;
import org.hurricane.Message;

public class TcpExample {
    public static void main(String[] args) throws Exception {
        Gateway gateway = new Gateway("localhost", 3000);

        while (true) {
            Message request = new Message();
            request.setType("request");
            request.setDestination("time_server");
            request.setTag(0);
            request.setData("");

            System.out.println(request);
            gateway.send(request);
            System.out.println(gateway.recv());
        }
    }
}
