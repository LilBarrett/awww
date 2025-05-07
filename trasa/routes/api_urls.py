from django.urls import path, include
from rest_framework.routers import DefaultRouter
from rest_framework_nested.routers import NestedDefaultRouter
from .api_views import RouteViewSet, PointViewSet

router = DefaultRouter()
router.register(r'routes', RouteViewSet, basename='api-routes')

points_router = NestedDefaultRouter(router, r'routes', lookup='route')
points_router.register(r'points', PointViewSet, basename='api-route-points')

urlpatterns = [
    path('', include(router.urls)),
    path('', include(points_router.urls)),
]
