import Data.Char

class MonadTrans t where
    lift :: Monad m => m a -> t m a

-- IdentityT
newtype IdentityT m a = IdentityT { runIdentityT :: m a}

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance (Functor m) => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT x) = IdentityT (fmap f x)

instance Monad m => Applicative (IdentityT m) where
    pure :: a -> IdentityT m a
    pure = lift . pure

    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    (IdentityT mf) <*> (IdentityT mx) = IdentityT (mf <*> mx)

instance Monad m => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT mx) >>= f = IdentityT $ mx >>= runIdentityT . f

-- EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
    lift :: Monad m => m a -> EitherT e m a
    lift = EitherT . fmap Right

instance (Functor m) => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT mea) = EitherT (fmap (fmap f) mea)

instance (Monad m) => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure = EitherT . pure . Right

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT mf) <*> (EitherT ma) = EitherT $ do
        ef <- mf
        case ef of
            Left e -> return (Left e)
            Right f -> fmap (fmap f) ma

instance (Monad m) => Monad (EitherT e m) where
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT mea) >>= f = EitherT $ do
        ea <- mea
        case ea of
            Left e  -> return (Left e)
            Right a -> runEitherT (f a)

-- ContT
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
    lift :: Monad m => m a -> ContT r m a
    lift m = ContT $ \cb -> m >>= cb

instance Functor (ContT r m) where
    fmap :: (a -> b) -> ContT r m a -> ContT r m b
    fmap f (ContT c) = ContT $ \cb -> c (cb . f)

instance Applicative (ContT r m) where
    pure :: a -> ContT r m a
    pure x = ContT ($ x)

    (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
    (ContT cf) <*> (ContT ca) = ContT $ \cb -> cf (\f -> ca (cb . f))

instance Monad (ContT r m) where
    (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
    (ContT ca) >>= f = ContT $ \cb -> ca (\a -> runContT (f a) cb)

readEmail :: IO (Either String String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Right str
    else return $ Left "Invalid email format"

readPassword :: IO (Either String String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8
    then return $ Left "Password must be at least 8 characters long"
    else if null (filter isUpper str)
      then return $ Left "Password must contain at least one uppercase letter"
      else if null (filter isLower str)
        then return $ Left "Password must contain at least one lowercase letter"
        else return $ Right str

readEmail' :: EitherT String IO String
readEmail' = EitherT readEmail

readPassword' :: EitherT String IO String
readPassword' = EitherT readPassword

signup' :: EitherT String IO (String, String)
signup' = do
  email <- readEmail'
  password <- readPassword'
  password2 <- readPassword'
  if password == password2
  then return (email, password)
  else EitherT . return $ Left "Passwords do not match"

main :: IO ()
main = do
  signupRes <- runEitherT signup'
  case signupRes of
    Left err -> putStrLn $ "Signup failed: " ++ err
    Right _ -> putStrLn "Signup success"
