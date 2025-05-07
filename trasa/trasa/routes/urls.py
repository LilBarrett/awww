from django.urls import path
from django.contrib.auth import views as auth_views
from . import views
from .views import LogoutViewAllowGet
from rest_framework.authtoken.views import obtain_auth_token

urlpatterns = [
    path('', views.route_list, name='route_list'),
    path('route/<int:pk>/', views.route_detail, name='route_detail'),
    path('routes/create/', views.route_create, name='route_create'),
    path('routes/<int:pk>/edit/', views.route_edit, name='route_edit'),
    path('register/', views.register, name='register'),
    path('routes/<int:route_pk>/delete_point/<int:point_pk>/', views.point_delete, name='point_delete'),
    path('login/', auth_views.LoginView.as_view(template_name='registration/login.html'), name='login'),
    path('logout/', LogoutViewAllowGet.as_view(), name='logout'),
]



