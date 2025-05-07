from django.contrib import admin
from .models import BackgroundImage, Route, Point

class PointInline(admin.TabularInline):
    model = Point
    extra = 1

@admin.register(Route)
class RouteAdmin(admin.ModelAdmin):
    list_display = ('name', 'user', 'background')
    inlines = [PointInline]

admin.site.register(BackgroundImage)
