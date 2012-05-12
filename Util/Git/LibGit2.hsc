{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Util.Git.LibGit2 where
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

-- I don't use #strict_import because it doesn't import allocaBytes and if
-- I import it separately I get tons of redundant import warnings.
import Foreign
import Foreign.C

#include <bindings.dsl.h>
#include <git2.h>


type Repo = Ptr C'git_repository

#opaque_t git_repository

-- int git_repository_init(git_repository **repo_out, const char *path,
-- unsigned is_bare);
#ccall git_repository_init, Ptr Repo -> CString -> CUInt -> IO Error
#ccall git_repository_open, Ptr Repo -> CString -> IO Error
#ccall git_repository_free, Repo -> IO ()

-- #opaque_t git_object
-- #ccall git_object_free, Ptr <git_object> -> IO ()

-- * blob

#opaque_t git_blob

#cinline git_blob_lookup, Ptr (Ptr <git_blob>) -> Repo -> Ptr OID -> IO Error
#cinline git_blob_free, Ptr <git_blob> -> IO ()

#ccall git_blob_create_frombuffer, Ptr OID -> Repo -> Ptr CChar -> CSize \
    -> IO Error
#ccall git_blob_rawcontent, Ptr <git_blob> -> IO (Ptr CChar)
#ccall git_blob_rawsize, Ptr <git_blob> -> IO CSize


-- * tree

#opaque_t git_tree
#opaque_t git_treebuilder
#opaque_t git_tree_entry

#cinline git_tree_lookup, Ptr (Ptr <git_tree>) -> Repo -> Ptr OID -> IO Error
#cinline git_tree_free, Ptr <git_tree> -> IO ()

-- const git_tree_entry * git_tree_entry_byindex(git_tree *tree, unsigned int
-- idx);
#ccall git_tree_entry_byindex, Ptr <git_tree> -> CUInt \
    -> IO (Ptr <git_tree_entry>)
-- unsigned int git_tree_entrycount(git_tree *tree);
#ccall git_tree_entrycount, Ptr <git_tree> -> IO CUInt
-- const char * git_tree_entry_name(const git_tree_entry *entry);
#ccall git_tree_entry_name, Ptr <git_tree_entry> -> IO CString
-- git_otype git_tree_entry_type(const git_tree_entry *entry);
#ccall git_tree_entry_type, Ptr <git_tree_entry> -> IO ObjType
-- const git_oid * git_tree_entry_id(const git_tree_entry *entry);
#ccall git_tree_entry_id, Ptr <git_tree_entry> -> IO (Ptr OID)

type ObjType = CInt
#enum ObjType, , GIT_OBJ_COMMIT, GIT_OBJ_TREE, GIT_OBJ_BLOB, GIT_OBJ_TAG


-- int git_treebuilder_create(git_treebuilder **builder_p, const git_tree
-- *source);
#ccall git_treebuilder_create, Ptr (Ptr <git_treebuilder>) -> Ptr <git_tree> \
    -> IO Error
-- int git_treebuilder_insert(git_tree_entry **entry_out, git_treebuilder *bld,
-- const char *filename, const git_oid *id, unsigned int attributes);
#ccall git_treebuilder_insert, Ptr (Ptr <git_tree_entry>) \
    -> Ptr <git_treebuilder> -> CString -> Ptr OID -> CUInt -> IO Error
-- int git_treebuilder_write(git_oid *oid, git_repository *repo,
-- git_treebuilder *bld);
#ccall git_treebuilder_write, Ptr OID -> Repo -> Ptr <git_treebuilder> \
    -> IO Error
-- void git_treebuilder_free(git_treebuilder *bld);
#ccall git_treebuilder_free, Ptr <git_treebuilder> -> IO ()


-- * commit

#opaque_t git_commit

#integral_t git_time_t

#starttype git_signature
#field name, CString
#field email, CString
#field when, <git_time_t>
#stoptype

#cinline git_commit_lookup, Ptr (Ptr <git_commit>) -> Repo -> Ptr OID \
    -> IO Error
#cinline git_commit_free, Ptr <git_commit> -> IO ()

-- int git_commit_create(git_oid *oid, git_repository *repo, const char
-- *update_ref, const git_signature *author, const git_signature *committer,
-- const char *message_encoding, const char *message, const git_tree *tree, int
-- parent_count, const git_commit *parents[]);
#ccall git_commit_create, Ptr OID -> Repo -> CString \
    -> Ptr <git_signature> -> Ptr <git_signature> \
    -> CString -> CString -> Ptr <git_tree> \
    -> CInt -> Ptr (Ptr <git_commit>) -> IO Error
#ccall git_signature_now, Ptr (Ptr <git_signature>) -> CString -> CString \
    -> IO Error
#ccall git_signature_free, Ptr <git_signature> -> IO ()

#ccall git_commit_author, Ptr <git_commit> -> IO (Ptr <git_signature>)
#ccall git_commit_tree_oid, Ptr  <git_commit> -> IO (Ptr OID)
#ccall git_commit_parentcount, Ptr <git_commit> -> IO CUInt
#ccall git_commit_parent_oid, Ptr <git_commit> -> CUInt -> IO (Ptr OID)
#ccall git_commit_message, Ptr <git_commit> -> IO CString


-- * OID

newtype OID = OID ByteString.ByteString deriving (Eq, Show)

instance Storable OID where
    sizeOf _ = #size git_oid
    alignment _ = 8
    poke oidp (OID bytes) =
        ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(bytesp, len) ->
            check ("OID poke " ++ show bytes) $
                c'git_oid_fromstrn oidp bytesp (fromIntegral len)
    peek oidp = allocaBytes 40 $ \bufferp -> do
        c'git_oid_fmt bufferp oidp
        OID `fmap` ByteString.packCStringLen (bufferp, 40)

#ccall git_oid_fromstrn, Ptr OID -> CString -> CSize -> IO Error
#ccall git_oid_fmt, CString -> Ptr OID -> IO ()

-- * error

type Error = CInt

error_msg :: Error -> Maybe String
error_msg errno
    | errno == (#const GIT_SUCCESS) = Nothing
    | otherwise = Just $ Map.findWithDefault
        ("undocumented errno: " ++ show errno) errno errors
    where
    errors = Map.fromList
        [ ((#const GIT_ERROR), "error")
        , ((#const GIT_ENOTFOUND), "not found")
        , ((#const GIT_EEXISTS), "already exists")
        , ((#const GIT_EOVERFLOW), "integer literal too large")
        , ((#const GIT_EAMBIGUOUS), "short oid is ambiguous")
        , ((#const GIT_EPASSTHROUGH), "passthrough")
        , ((#const GIT_ESHORTBUFFER), "buffer to short to satisfy request")
        , ((#const GIT_EREVWALKOVER), "rev walk over (undocumented error)")
        ]

newtype GitException = GitException String deriving (Typeable.Typeable)
instance Exception.Exception GitException
instance Show GitException where
    show (GitException msg) = "GitException: " ++ msg

throw :: String -> IO a
throw = Exception.throwIO . GitException

check :: String -> IO Error -> IO ()
check caller action = do
    errno <- action
    case error_msg errno of
        Nothing -> return ()
        Just msg -> throw $ caller ++ ": " ++ msg
