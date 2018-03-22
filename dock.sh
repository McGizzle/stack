docker build -t mystack-base -f Dockerfile.base .
echo "Building..."
stack --docker build
echo "Creating image container..."
stack --docker image container
