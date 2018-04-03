echo "Building image.."
docker build -t mystack-base -f Dockerfile.base .
echo "Building Stack..."
stack --docker build
echo "Creating image container..."
stack --docker image container
echo "Done diddly done :)"
