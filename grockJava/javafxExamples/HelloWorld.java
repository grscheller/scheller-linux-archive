import javafx.application.Application;
import javafx.stage.Stage;
import javafx.scene.Group;
import javafx.scene.text.Text;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;

public class HelloWorld extends Application {         
    public static void main(String[] args) {
        launch(args);             
    }             
    @Override public void start(Stage primaryStage) {
        primaryStage.setTitle("Hello World!");
        Group root = new Group(); 
        Scene scene = new Scene(root, 400, 250, Color.ALICEBLUE);
        Text text = new Text();
        text.setX(105);
        text.setY(120);
        text.setFont(new Font(30));
        text.setText("Hello World");
        root.getChildren().add(text);
        primaryStage.setScene(scene);   
        primaryStage.show();
    }
}
