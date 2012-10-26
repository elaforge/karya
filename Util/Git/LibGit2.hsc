{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Util.Git.LibGit2 where
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable
import qualified Data.Word as Word

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

-- ** treebuilder

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
-- int git_treebuilder_remove(git_treebuilder *bld, const char *filename);
#ccall git_treebuilder_remove, Ptr <git_treebuilder> -> CString -> IO Error
-- const git_tree_entry git_treebuilder_get(git_treebuilder *bld,
-- const char *filename);
#ccall git_treebuilder_get, Ptr <git_treebuilder> -> CString \
    -> IO (Ptr <git_tree_entry>)
-- int git_treebuilder_write(git_oid *oid, git_repository *repo,
-- git_treebuilder *bld);
#ccall git_treebuilder_write, Ptr OID -> Repo -> Ptr <git_treebuilder> \
    -> IO Error
-- void git_treebuilder_free(git_treebuilder *bld);
#ccall git_treebuilder_free, Ptr <git_treebuilder> -> IO ()

-- ** index

-- int git_tree_create_fromindex(git_oid *oid, git_index *index);
#ccall git_tree_create_fromindex, Ptr OID -> Ptr <git_index> -> IO Error

-- * diff

#opaque_t git_diff_list
#opaque_t git_diff_options

-- int git_diff_tree_to_tree(git_repository *repo, const git_diff_options
-- *opts, git_tree *old_tree, git_tree *new_tree, git_diff_list **diff);
#ccall git_diff_tree_to_tree, Repo -> Ptr <git_diff_options> \
    -> Ptr <git_tree> -> Ptr <git_tree> -> Ptr (Ptr <git_diff_list>) \
    -> IO Error

#ccall git_diff_list_free, Ptr <git_diff_list> -> IO ()

-- int git_diff_print_compact( git_diff_list *diff, void *cb_data,
-- git_diff_data_fn print_cb);
#ccall git_diff_print_compact, Ptr <git_diff_list> -> Ptr CChar \
    -> <git_diff_data_fn> -> IO Error

#opaque_t git_diff_range

-- typedef int (*git_diff_data_fn)(
--         void *cb_data,
--         git_diff_delta *delta,
--         git_diff_range *range,
--         char line_origin, /**< GIT_DIFF_LINE_... value from above */
--         const char *content,
--         size_t content_len);
#callback git_diff_data_fn, Ptr CChar -> Ptr <git_diff_delta> \
    -> Ptr <git_diff_range> -> CChar -> CString -> CSize -> IO Error

#integral_t git_delta_t
#num GIT_DELTA_ADDED
#num GIT_DELTA_DELETED
#num GIT_DELTA_MODIFIED

#starttype git_diff_delta
#field old_file, <git_diff_file>
#field new_file, <git_diff_file>
#field status, <git_delta_t>
#stoptype

#starttype git_diff_file
#field oid, OID
#field path, CString
#field mode, Word.Word16
-- #field size, <git_off_t>
-- #field flags, CUInt
#stoptype

-- * index

#opaque_t git_index

-- int git_index_read_tree(git_index *index, git_tree *tree);
#ccall git_index_read_tree, Ptr <git_index> -> Ptr <git_tree> -> IO Error
-- void git_index_free(git_index *index);
#ccall git_index_free, Ptr <git_index> -> IO ()

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

-- * ref

#opaque_t git_reference

-- int git_reference_lookup(git_reference **reference_out, git_repository
-- *repo, const char *name);
#ccall git_reference_lookup, Ptr (Ptr <git_reference>) -> Repo -> CString \
    -> IO Error
-- void git_reference_free(git_reference *ref);
#ccall git_reference_free, Ptr <git_reference> -> IO ()

-- int git_reference_name_to_oid(git_oid *out, git_repository *repo, const char
-- *name);
#ccall git_reference_name_to_oid, Ptr OID -> Repo -> CString -> IO CInt

-- int git_reference_create_oid(git_reference **ref_out, git_repository *repo,
-- const char *name, const git_oid *id, int force);
#ccall git_reference_create_oid, Ptr (Ptr <git_reference>) -> Repo \
    -> CString -> Ptr OID -> CInt -> IO Error
-- const char * git_reference_name(git_reference *ref);
#ccall git_reference_name, Ptr <git_reference> -> IO CString
-- const git_oid * git_reference_oid(git_reference *ref);
#ccall git_reference_oid, Ptr <git_reference> -> IO (Ptr OID)

-- int git_reference_list(git_strarray *array, git_repository *repo, unsigned
-- int list_flags);
#ccall git_reference_list, Ptr <git_strarray> -> Repo -> CUInt -> IO Error

-- ** symbolic

-- int git_reference_create_symbolic(git_reference **ref_out, git_repository
-- *repo, const char *name, const char *target, int force);
#ccall git_reference_create_symbolic, Ptr (Ptr <git_reference>) -> Repo \
    -> CString -> CString -> CInt -> IO Error
-- int git_reference_resolve(git_reference **resolved_ref, git_reference *ref);
#ccall git_reference_resolve, Ptr (Ptr <git_reference>) \
    -> Ptr <git_reference> -> IO Error

-- * strarray

#starttype git_strarray
#field strings, Ptr CString
#field count, CSize
#stoptype

#num GIT_REF_LISTALL

#ccall git_strarray_free, Ptr <git_strarray> -> IO ()

-- * revwalk

#opaque_t git_revwalk

-- int git_revwalk_new(git_revwalk **walker, git_repository *repo);
#ccall git_revwalk_new, Ptr (Ptr <git_revwalk>) -> Repo -> IO Error
-- void git_revwalk_free(git_revwalk *walk);
#ccall git_revwalk_free, Ptr <git_revwalk> -> IO ()

-- int git_revwalk_push(git_revwalk *walk, const git_oid *oid);
#ccall git_revwalk_push, Ptr <git_revwalk> -> Ptr OID -> IO Error
-- int git_revwalk_push_ref(git_revwalk *walk, const char *refname);
#ccall git_revwalk_push_ref, Ptr <git_revwalk> -> CString -> IO Error
-- void git_revwalk_sorting(git_revwalk *walk, unsigned int sort_mode);
#ccall git_revwalk_sorting, Ptr <git_revwalk> -> SortMode -> IO ()
-- int git_revwalk_next(git_oid *oid, git_revwalk *walk);
#ccall git_revwalk_next, Ptr OID -> Ptr <git_revwalk> -> IO Error

type SortMode = CUInt
#enum SortMode, , GIT_SORT_NONE, GIT_SORT_TOPOLOGICAL, GIT_SORT_TIME, \
    GIT_SORT_REVERSE

-- * OID

newtype OID = OID ByteString.ByteString deriving (Eq, Ord, Show)

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

show_oid :: OID -> String
show_oid (OID bytes) = ByteString.unpack bytes

read_oid :: ByteString.ByteString -> OID
read_oid = OID . ByteString.takeWhile (not . Char.isSpace)
    . ByteString.dropWhile Char.isSpace

-- * error

type Error = CInt

#integral_t git_error_t
#num GIT_OK
#num GIT_ERROR
#num GIT_ENOTFOUND
#num GIT_EEXISTS
#num GIT_ITEROVER

error_msg :: Error -> Maybe String
error_msg errno
    | errno == (#const GIT_OK) = Nothing
    | otherwise = Just $ Map.findWithDefault
        ("undocumented errno: " ++ show errno) errno errors
    where
    errors = Map.fromList
        [ ((#const GIT_ERROR), "error")
        , ((#const GIT_ENOTFOUND), "not found")
        , ((#const GIT_EEXISTS), "already exists")
        , ((#const GIT_EAMBIGUOUS), "short oid is ambiguous")
        , ((#const GIT_EBUFS), "undocumented GIT_EBUFS")
        , ((#const GIT_PASSTHROUGH), "passthrough")
        , ((#const GIT_ITEROVER), "iteration over")
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

check_lookup :: String -> Ptr (Ptr a) -> IO Error -> IO (Ptr a)
check_lookup caller ptrptr io = do
    errno <- io
    if errno == c'GIT_ENOTFOUND then return nullPtr else do
    case error_msg errno of
        Nothing -> peek ptrptr
        Just msg -> throw $ caller ++ ": " ++ msg
