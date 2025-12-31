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

package de.markusbordihn.easynpc.client.renderer.entity.raw;

import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.data.skin.variant.HorseSkinVariant;
import net.minecraft.client.model.animal.equine.EquineSaddleModel;
import net.minecraft.client.model.animal.equine.HorseModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.AbstractHorseRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.HorseMarkingLayer;
import net.minecraft.client.renderer.entity.layers.SimpleEquipmentLayer;
import net.minecraft.client.renderer.entity.state.HorseRenderState;
import net.minecraft.client.resources.model.EquipmentClientInfo.LayerType;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.animal.equine.Horse;

public class HorseRawRenderer extends AbstractHorseRenderer<Horse, HorseRenderState, HorseModel>
    implements EasyNPCEntityRenderer {

  protected static final Identifier DEFAULT_TEXTURE = HorseSkinVariant.WHITE.getTextureLocation();

  public HorseRawRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new HorseModel(context.bakeLayer(ModelLayers.HORSE)),
        new HorseModel(context.bakeLayer(ModelLayers.HORSE_BABY)));
    this.addLayer(new HorseMarkingLayer(this));
    this.addLayer(
        new SimpleEquipmentLayer<>(
            this,
            context.getEquipmentRenderer(),
            LayerType.HORSE_BODY,
            (renderState) -> renderState.bodyArmorItem,
            new HorseModel(context.bakeLayer(ModelLayers.HORSE_ARMOR)),
            new HorseModel(context.bakeLayer(ModelLayers.HORSE_BABY_ARMOR))));
    this.addLayer(
        new SimpleEquipmentLayer<>(
            this,
            context.getEquipmentRenderer(),
            LayerType.HORSE_SADDLE,
            (renderState) -> renderState.saddle,
            new EquineSaddleModel(context.bakeLayer(ModelLayers.HORSE_SADDLE)),
            new EquineSaddleModel(context.bakeLayer(ModelLayers.HORSE_BABY_SADDLE))));
  }

  public Identifier getTextureLocation(HorseRenderState renderState) {
    return getTextureFromRenderState(renderState);
  }

  public HorseRenderState createRenderState() {
    return new HorseRenderState();
  }

  @Override
  public void extractRenderState(Horse horse, HorseRenderState state, float partialTicks) {
    super.extractRenderState(horse, state, partialTicks);
    state.variant = horse.getVariant();
    state.markings = horse.getMarkings();
    state.bodyArmorItem = horse.getBodyArmorItem().copy();
  }

  @Override
  public Identifier getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
