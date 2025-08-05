import threading

class SingletonMeta(type):
    """
    A metaclass to implement the Singleton pattern, ensuring thread-safety.
    """
    _instances = {}
    _lock = threading.Lock() # For thread-safe instance creation

    def __call__(cls, *args, **kwargs):
        """
        This method is called when you try to create an instance of a class
        that uses this metaclass.
        """
        # Double-checked locking for thread safety
        if cls not in cls._instances:
            with cls._lock:
                if cls not in cls._instances:
                    # Create the instance if it doesn't exist
                    # super().__call__ refers to the type.__call__ method,
                    # which is responsible for calling __new__ and then __init__
                    cls._instances[cls] = super().__call__(*args, **kwargs)
        return cls._instances[cls]

class Logger(metaclass=SingletonMeta):
    """
    A class that uses the SingletonMeta to ensure only one instance exists.
    """
    def __init__(self, name="MyLogger"):
        # __init__ will only be called once when the instance is first created
        if not hasattr(self, '_initialized'): # Use a flag to ensure init logic runs once
            self.name = name
            print(f"Logger initialized: {self.name}")
            self._initialized = True

    def log(self, message):
        print(f"[{self.name}] {message}")

# --- Usage ---
logger1 = Logger("AppLogger")
logger2 = Logger("AnotherLogger") # This will return the same instance as logger1

logger1.log("This is a log message from logger1.")
logger2.log("This is a log message from logger2.")

print(logger1 is logger2) # Output: True

# You can even try with different arguments, but the __init__ will only run once
logger3 = Logger("YetAnotherLogger")
print(logger1 is logger3) # Output: True
logger3.log("Logging from logger3.")

# Verify the name set during the first initialization
print(logger3.name) # Output: AppLogger (not YetAnotherLogger)

