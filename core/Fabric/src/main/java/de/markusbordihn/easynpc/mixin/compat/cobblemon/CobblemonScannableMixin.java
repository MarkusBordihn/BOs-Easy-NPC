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

package de.markusbordihn.easynpc.mixin.compat.cobblemon;

import com.cobblemon.mod.common.pokedex.scanner.PokedexEntityData;
import com.cobblemon.mod.common.pokedex.scanner.ScannableEntity;
import com.cobblemon.mod.common.pokemon.Pokemon;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonPokemonResolver;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonSpeciesManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.cobblemon.CobblemonNPC;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;

@Mixin(CobblemonNPC.class)
public class CobblemonScannableMixin implements ScannableEntity {

  @Override
  public PokedexEntityData resolvePokemonScan() {
    EasyNPC<?> easyNpc = (EasyNPC<?>) (Object) this;
    RenderDataCapable<?> renderData = easyNpc.getEasyNPCRenderData();
    if (renderData == null
        || renderData.getRenderDataEntry() == null
        || renderData.getRenderDataEntry().getRenderType() != RenderType.COBBLEMON_ENTITY) {
      return null;
    }
    String modelString = renderData.getRenderDataEntry().getRenderEntityModel();
    if (modelString == null || modelString.isEmpty()) {
      modelString = CobblemonSpeciesManager.DEFAULT_MODEL;
    }
    ResourceLocation modelKey = ResourceLocation.tryParse(modelString);
    if (modelKey == null) {
      return null;
    }
    Pokemon pokemon = CobblemonPokemonResolver.resolvePokemon(modelKey);
    if (pokemon == null) {
      return null;
    }
    return new PokedexEntityData(pokemon, null);
  }

  @Override
  public LivingEntity resolveEntityScan() {
    return (LivingEntity) (Object) this;
  }
}
