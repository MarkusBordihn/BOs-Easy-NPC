package de.markusbordihn.easynpc.client.renderer.entity.custom;

import de.markusbordihn.easynpc.client.model.custom.FairyModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.data.skin.variant.FairySkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Fairy;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.resources.Identifier;

public class FairyRenderer
    extends HumanoidMobRenderer<Fairy, HumanoidRenderState, FairyModel<HumanoidRenderState>>
    implements EasyNPCEntityRenderer {

  protected static final Identifier DEFAULT_TEXTURE = FairySkinVariant.GREEN.getTextureLocation();

  public FairyRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new FairyModel<>(context.bakeLayer(modelLayerLocation)), 0.3F);
  }

  @Override
  public Identifier getTextureLocation(HumanoidRenderState renderState) {
    return getTextureFromRenderState(renderState);
  }

  @Override
  public Identifier getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public HumanoidRenderState createRenderState() {
    return new HumanoidRenderState();
  }
}
