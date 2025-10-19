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

package de.markusbordihn.easynpc.client.renderer.entity;

import de.markusbordihn.easynpc.client.model.ModModelLayers;
import de.markusbordihn.easynpc.client.renderer.entity.custom.DopplerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.custom.FairyRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.custom.OrcRenderer;
import de.markusbordihn.easynpc.entity.ModCustomEntityType;
import java.util.function.Function;
import java.util.function.Supplier;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider.Context;
import net.minecraft.world.entity.Entity;

public enum ModCustomEntityRenderer {
  DOPPLER(
      ModCustomEntityType.DOPPLER,
      () -> context -> new DopplerRenderer<>(context, ModModelLayers.DOPPLER)),
  FAIRY(
      ModCustomEntityType.FAIRY,
      () -> context -> new FairyRenderer<>(context, ModModelLayers.FAIRY)),
  ORC(ModCustomEntityType.ORC, () -> context -> new OrcRenderer<>(context, ModModelLayers.ORC)),
  ORC_WARRIOR(
      ModCustomEntityType.ORC_WARRIOR,
      () -> context -> new OrcRenderer<>(context, ModModelLayers.ORC));

  private final ModCustomEntityType entityType;
  private final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer;

  ModCustomEntityRenderer(
      final ModCustomEntityType entityType,
      final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer) {
    this.entityType = entityType;
    this.renderer = renderer;
  }

  public ModCustomEntityType getEntityType() {
    return entityType;
  }

  public Function<Context, EntityRenderer<? extends Entity>> getRenderer() {
    return renderer.get();
  }
}
