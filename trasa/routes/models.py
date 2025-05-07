from django.contrib.auth.models import User
from django.db import models

class BackgroundImage(models.Model):
    name = models.CharField(max_length=100)
    image = models.ImageField(upload_to='backgrounds/')

    def __str__(self):
        return self.name

class Route(models.Model):
    name = models.CharField(max_length=100)
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    background = models.ForeignKey(BackgroundImage, on_delete=models.CASCADE)

    def __str__(self):
        return f"{self.name} ({self.user.username})"

class Point(models.Model):
    route = models.ForeignKey(Route, related_name='points', on_delete=models.CASCADE)
    x = models.FloatField()
    y = models.FloatField()
    order = models.PositiveIntegerField()

    def __str__(self):
        return f"({self.x}, {self.y}) in {self.route.name}"
