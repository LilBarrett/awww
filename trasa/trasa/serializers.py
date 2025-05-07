from rest_framework import serializers
from .models import Trasa, PunktTrasy

class TrasaSerializer(serializers.ModelSerializer):
    class Meta:
        model = Trasa
        fields = '__all__'

class PunktTrasySerializer(serializers.ModelSerializer):
    class Meta:
        model = PunktTrasy
        fields = '__all__'
