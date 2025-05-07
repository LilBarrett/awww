from rest_framework import serializers
from .models import Route, Point, BackgroundImage

class BackgroundImageSerializer(serializers.ModelSerializer):
    image_url = serializers.ImageField(source='image', read_only=True)

    class Meta:
        model = BackgroundImage
        fields = ['id', 'name', 'image_url']

class RouteSerializer(serializers.ModelSerializer):
    # background can be provided as an ID
    background = serializers.PrimaryKeyRelatedField(queryset=BackgroundImage.objects.all())

    class Meta:
        model = Route
        fields = ['id', 'name', 'background']  # user is set automatically in the viewset

class PointSerializer(serializers.ModelSerializer):
    class Meta:
        model = Point
        fields = ['id', 'route', 'x', 'y', 'order']
