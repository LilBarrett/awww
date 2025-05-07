from rest_framework import viewsets, permissions
from rest_framework.exceptions import PermissionDenied
from .models import Route, Point
from .serializers import RouteSerializer, PointSerializer

class RouteViewSet(viewsets.ModelViewSet):
    serializer_class = RouteSerializer
    permission_classes = [permissions.IsAuthenticated]

    def get_queryset(self):
        # Only return routes owned by the current user
        return Route.objects.filter(user=self.request.user)

    def perform_create(self, serializer):
        # Assign the route to the current user
        serializer.save(user=self.request.user)

class PointViewSet(viewsets.ModelViewSet):
    serializer_class = PointSerializer
    permission_classes = [permissions.IsAuthenticated]

    def get_queryset(self):
        # Only show points for routes that belong to the user
        qs = Point.objects.filter(route__user=self.request.user)
        route_pk = self.kwargs.get('route_pk')
        if route_pk:
            qs = qs.filter(route_id=route_pk)
        return qs

    def perform_create(self, serializer):
        # Use the nested URL to determine the route, and prevent user from adding points to others' routes
        route_pk = self.kwargs.get('route_pk')
        if route_pk:
            try:
                route = Route.objects.get(pk=route_pk, user=self.request.user)
            except Route.DoesNotExist:
                raise PermissionDenied("You cannot add points to someone else's route.")
            serializer.save(route=route)
        else:
            # Fallback for non-nested usage, still enforce ownership
            route = serializer.validated_data['route']
            if route.user != self.request.user:
                raise PermissionDenied("You cannot add points to someone else's route.")
            serializer.save()
