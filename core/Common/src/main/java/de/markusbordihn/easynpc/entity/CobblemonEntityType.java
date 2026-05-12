/*
 * Copyright 2023 Markus Bordihn
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
import de.markusbordihn.easynpc.entity.easynpc.npc.cobblemon.CobblemonNPC;
import java.util.function.Supplier;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.level.Level;

public enum CobblemonEntityType implements ModEntityTypeProvider {
  COBBLEMON_NPC(
      CobblemonNPC.ID,
      EntityType.Builder.of(
              (EntityType<CobblemonNPC> type, Level level) -> new CobblemonNPC(type, level),
              MobCategory.CREATURE)
          .sized(0.6F, 1.8F)
          .clientTrackingRange(12),
      CobblemonNPC::createAttributes);

  private final String id;
  private final EntityType.Builder<? extends Entity> builder;
  private final Supplier<AttributeSupplier.Builder> attributes;
  private final ResourceKey<EntityType<?>> resourceKey;

  CobblemonEntityType(
      String id,
      EntityType.Builder<? extends PathfinderMob> builder,
      Supplier<AttributeSupplier.Builder> attributes) {
    this.id = id;
    this.builder = builder;
    this.attributes = attributes;
    this.resourceKey =
        ResourceKey.create(
            Registries.ENTITY_TYPE, ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, id));
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public EntityType.Builder<? extends Entity> getBuilder() {
    return builder;
  }

  @Override
  public ResourceKey<EntityType<?>> getResourceKey() {
    return resourceKey;
  }

  public AttributeSupplier.Builder getAttributes() {
    return attributes.get();
  }
}
