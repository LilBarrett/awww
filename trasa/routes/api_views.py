from rest_framework import viewsets, permissions
from .models import Route, Point
from .serializers import RouteSerializer, PointSerializer

class RouteViewSet(viewsets.ModelViewSet):
    serializer_class = RouteSerializer
    permission_classes = [permissions.IsAuthenticated]
    def get_queryset(self):
        return Route.objects.filter(user=self.request.user)
    def perform_create(self, serializer):
        serializer.save(user=self.request.user)

class PointViewSet(viewsets.ModelViewSet):
    serializer_class = PointSerializer
    permission_classes = [permissions.IsAuthenticated]
    def get_queryset(self):
        # Only show points for routes that belong to the user
        return Point.objects.filter(route__user=self.request.user)
    def perform_create(self, serializer):
        # Let the user specify the route (must belong to them)
        route = serializer.validated_data['route']
        if route.user != self.request.user:
            raise PermissionError("You cannot add points to someone else's route.")
        serializer.save()
