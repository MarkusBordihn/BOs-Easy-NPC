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

package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.client.model.standard.StandardWolfModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.HeldItemLayer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Wolf;
import de.markusbordihn.easynpc.entity.easynpc.npc.Wolf.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;

public class WolfModelRenderer
    extends BaseMobModelRenderer<Wolf, Variant, StandardWolfModel<Wolf>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.DEFAULT,
                ResourceLocation.withDefaultNamespace("textures/entity/wolf/wolf.png"));
            map.put(
                Variant.TAMED,
                ResourceLocation.withDefaultNamespace("textures/entity/wolf/wolf_tame.png"));
            map.put(
                Variant.ANGRY,
                ResourceLocation.withDefaultNamespace("textures/entity/wolf/wolf_angry.png"));
          });

  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.DEFAULT);

  public WolfModelRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new StandardWolfModel<>(context.bakeLayer(ModelLayers.WOLF)),
        0.7F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(new HeldItemLayer<>(this, context.getItemInHandRenderer()));
  }
}
