from django.contrib import admin
from django.urls import path, include
from django.conf import settings
from django.conf.urls.static import static
from drf_yasg.views import get_schema_view
from drf_yasg import openapi
from rest_framework import permissions
from rest_framework.authtoken.views import obtain_auth_token  
from rest_framework.authentication import TokenAuthentication

schema_view = get_schema_view(
   openapi.Info(
      title="Routes API",
      default_version='v1',
      description="API documentation for your routes and points",
   ),
   public=True,
   permission_classes=(permissions.AllowAny,),
   authentication_classes=(TokenAuthentication,),  # <--- ADD THIS
)

urlpatterns = [
    path('admin/', admin.site.urls),
    path('', include('routes.urls')),
    path('api/', include('routes.api_urls')),
    path('swagger/', schema_view.with_ui('swagger', cache_timeout=0), name='schema-swagger-ui'),
    path('api-token-auth/', obtain_auth_token, name='api_token_auth'),  # ADD THIS LINE
]

if settings.DEBUG:
    urlpatterns += static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
