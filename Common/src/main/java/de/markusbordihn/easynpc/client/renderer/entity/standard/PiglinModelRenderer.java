package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.client.model.standard.StandardPiglinModel;
import de.markusbordihn.easynpc.client.renderer.EasyNPCModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseLivingEntityModelRenderer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Piglin;
import de.markusbordihn.easynpc.entity.easynpc.npc.Piglin.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ElytraLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.resources.ResourceLocation;

public class PiglinModelRenderer
    extends BaseLivingEntityModelRenderer<Piglin, Variant, StandardPiglinModel<Piglin>> {

  protected static final Map<Piglin.Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Piglin.Variant.class),
          map -> {
            map.put(
                Piglin.Variant.DEFAULT, new ResourceLocation("textures/entity/piglin/piglin.png"));
            map.put(
                Piglin.Variant.BRUTE,
                new ResourceLocation("textures/entity/piglin/piglin_brute.png"));
            map.put(
                Piglin.Variant.ZOMBIFIED,
                new ResourceLocation("textures/entity/piglin/zombified_piglin.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT.get(Piglin.Variant.DEFAULT);

  public <T extends RenderLayer<Piglin, StandardPiglinModel<Piglin>>> PiglinModelRenderer(
      EntityRendererProvider.Context context, Class<T> humanoidArmorLayerClass) {
    super(
        context,
        new StandardPiglinModel<>(context.bakeLayer(ModelLayers.PIGLIN)),
        0.5F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(
        EasyNPCModelRenderer.getHumanoidArmorLayer(
            this,
            context,
            ModelLayers.ZOMBIE_INNER_ARMOR,
            ModelLayers.ZOMBIE_OUTER_ARMOR,
            humanoidArmorLayerClass));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ElytraLayer<>(this, context.getModelSet()));
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
  }
}
