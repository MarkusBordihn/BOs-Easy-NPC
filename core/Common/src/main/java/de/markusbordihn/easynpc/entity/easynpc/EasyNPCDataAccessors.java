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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.CustomAttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundData;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttributeHandler;
import net.minecraft.world.entity.PathfinderMob;

public interface EasyNPCDataAccessors<E extends PathfinderMob> {

  default ActionEventData<E> getEasyNPCActionEventData() {
    return this instanceof ActionEventData<E> actionEventData ? actionEventData : null;
  }

  default AttackData<E> getEasyNPCAttackData() {
    return this instanceof AttackData<E> attackData ? attackData : null;
  }

  default AttributeData<E> getEasyNPCAttributeData() {
    return this instanceof AttributeData<E> attributeData ? attributeData : null;
  }

  default ConfigData<E> getEasyNPCConfigData() {
    return this instanceof ConfigData<E> configData ? configData : null;
  }

  default ConfigurationData<E> getEasyNPCConfigurationData() {
    return this instanceof ConfigurationData<E> configurationData ? configurationData : null;
  }

  default CustomAttributeData<E> getEasyNPCCustomAttributeData() {
    return this instanceof CustomAttributeData<E> customAttributeData ? customAttributeData : null;
  }

  default DialogData<E> getEasyNPCDialogData() {
    return this instanceof DialogData<E> dialogData ? dialogData : null;
  }

  default DisplayAttributeData<E> getEasyNPCDisplayAttributeData() {
    return this instanceof DisplayAttributeData<E> displayAttributeData
        ? displayAttributeData
        : null;
  }

  default GuiData<E> getEasyNPCGuiData() {
    return this instanceof GuiData<E> guiData ? guiData : null;
  }

  default SkinData<E> getEasyNPCSkinData() {
    return this instanceof SkinData<E> skinData ? skinData : null;
  }

  default ModelData<E> getEasyNPCModelData() {
    return this instanceof ModelData<E> modelData ? modelData : null;
  }

  default NavigationData<E> getEasyNPCNavigationData() {
    return this instanceof NavigationData<E> navigationData ? navigationData : null;
  }

  default ObjectiveData<E> getEasyNPCObjectiveData() {
    return this instanceof ObjectiveData<E> objectiveData ? objectiveData : null;
  }

  default OwnerData<E> getEasyNPCOwnerData() {
    return this instanceof OwnerData<E> ownerData ? ownerData : null;
  }

  default PresetData<E> getEasyNPCPresetData() {
    return this instanceof PresetData<E> presetData ? presetData : null;
  }

  default ProfessionData<E> getEasyNPCProfessionData() {
    return this instanceof ProfessionData<E> professionData ? professionData : null;
  }

  default RenderData<E> getEasyNPCRenderData() {
    return this instanceof RenderData<E> renderData ? renderData : null;
  }

  default ServerData<E> getEasyNPCServerData() {
    return this instanceof ServerData<E> serverData ? serverData : null;
  }

  default TickerData<E> getEasyNPCTickerData() {
    return this instanceof TickerData<E> tickerData ? tickerData : null;
  }

  default TradingData<E> getEasyNPCTradingData() {
    return this instanceof TradingData<E> tradingData ? tradingData : null;
  }

  default SoundData<E> getEasyNPCSoundData() {
    return this instanceof SoundData<E> soundData ? soundData : null;
  }

  default VariantData<E> getEasyNPCVariantData() {
    return this instanceof VariantData<E> variantData ? variantData : null;
  }

  default ActionHandler<E> getEasyNPCActionHandler() {
    return this instanceof ActionHandler<E> actionHandler ? actionHandler : null;
  }

  default AttributeHandler<E> getEasyNPCAttributeHandler() {
    return this instanceof AttributeHandler<E> attributeHandler ? attributeHandler : null;
  }
}
