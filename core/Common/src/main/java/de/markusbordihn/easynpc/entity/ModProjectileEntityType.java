/*
 * Copyright 2025 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCBullet;
import de.markusbordihn.easynpc.entity.projectile.EasyNPCItemProjectile;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.level.Level;

public enum ModProjectileEntityType implements ModEntityTypeProvider {
  BULLET(
      EasyNPCBullet.ID,
      EntityType.Builder.of(
              (EntityType<EasyNPCBullet> type, Level level) -> new EasyNPCBullet(type, level),
              MobCategory.MISC)
          .sized(0.25F, 0.25F)
          .clientTrackingRange(4)
          .updateInterval(20)),
  ITEM_PROJECTILE(
      EasyNPCItemProjectile.ID,
      EntityType.Builder.of(
              (EntityType<EasyNPCItemProjectile> type, Level level) ->
                  new EasyNPCItemProjectile(type, level),
              MobCategory.MISC)
          .sized(0.25F, 0.25F)
          .clientTrackingRange(4)
          .updateInterval(10));

  private final String id;
  private final EntityType.Builder<? extends Entity> builder;
  private final ResourceKey<EntityType<?>> resourceKey;

  ModProjectileEntityType(final String id, final EntityType.Builder<? extends Entity> builder) {
    this.id = id;
    this.builder = builder;
    this.resourceKey =
        ResourceKey.create(
            Registries.ENTITY_TYPE, Identifier.fromNamespaceAndPath(Constants.MOD_ID, id));
  }

  @Override
  public String getId() {
    return this.id;
  }

  @Override
  public EntityType.Builder<? extends Entity> getBuilder() {
    return this.builder;
  }

  @Override
  public ResourceKey<EntityType<?>> getResourceKey() {
    return this.resourceKey;
  }

  @Override
  public AttributeSupplier.Builder getAttributes() {
    return null;
  }
}
