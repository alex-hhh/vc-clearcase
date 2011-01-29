release=$1
release_tag=`echo $release | sed 's/\./-/'`
repository="`pwd`/code"
release_dir="`pwd`/release"

if [ -z "$release_tag" ]; then
    echo "Bad release name"
    exit 1;
fi

if ! [ -e "$repository" ]; then
    echo "Repository $repository does not exist."
    exit 1;
fi

if ! [ -d "$repository" ]; then
    echo "Bad repository (not a directory): $repository";
    exit 1;
fi

if ! [ -e "$release_dir" ]; then
    echo "Release Dir $release_dir does not exist."
    exit 1;
fi

if ! [ -d "$release_dir" ]; then
    echo "Bad release dir (not a directory): $release_dir";
    exit 1;
fi

hg --cwd $repository tag -m "Tagged release $release" vc-clearcase-$release_tag
hg --cwd $repository archive $release_dir/vc-clearcase-$release

