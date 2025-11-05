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

import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttributeHandler;
import net.minecraft.world.entity.PathfinderMob;

public interface EasyNPCDataAccessors<E extends PathfinderMob> {

  default ActionEventDataCapable<E> getEasyNPCActionEventData() {
    return this instanceof ActionEventDataCapable<E> actionEventData ? actionEventData : null;
  }

  default AttackDataCapable<E> getEasyNPCAttackData() {
    return this instanceof AttackDataCapable<E> attackData ? attackData : null;
  }

  default AttributeDataCapable<E> getEasyNPCAttributeData() {
    return this instanceof AttributeDataCapable<E> attributeData ? attributeData : null;
  }

  default ConfigDataCapable<E> getEasyNPCConfigData() {
    return this instanceof ConfigDataCapable<E> configData ? configData : null;
  }

  default ConfigurationDataCapable<E> getEasyNPCConfigurationData() {
    return this instanceof ConfigurationDataCapable<E> configurationData ? configurationData : null;
  }

  default DialogDataCapable<E> getEasyNPCDialogData() {
    return this instanceof DialogDataCapable<E> dialogData ? dialogData : null;
  }

  default DisplayAttributeDataCapable<E> getEasyNPCDisplayAttributeData() {
    return this instanceof DisplayAttributeDataCapable<E> displayAttributeData
        ? displayAttributeData
        : null;
  }

  default SkinDataCapable<E> getEasyNPCSkinData() {
    return this instanceof SkinDataCapable<E> skinData ? skinData : null;
  }

  default ModelDataCapable<E> getEasyNPCModelData() {
    return this instanceof ModelDataCapable<E> modelData ? modelData : null;
  }

  default NavigationDataCapable<E> getEasyNPCNavigationData() {
    return this instanceof NavigationDataCapable<E> navigationData ? navigationData : null;
  }

  default ObjectiveDataCapable<E> getEasyNPCObjectiveData() {
    return this instanceof ObjectiveDataCapable<E> objectiveData ? objectiveData : null;
  }

  default OwnerDataCapable<E> getEasyNPCOwnerData() {
    return this instanceof OwnerDataCapable<E> ownerData ? ownerData : null;
  }

  default PresetDataCapable<E> getEasyNPCPresetData() {
    return this instanceof PresetDataCapable<E> presetData ? presetData : null;
  }

  default ProfessionDataCapable<E> getEasyNPCProfessionData() {
    return this instanceof ProfessionDataCapable<E> professionData ? professionData : null;
  }

  default RenderDataCapable<E> getEasyNPCRenderData() {
    return this instanceof RenderDataCapable<E> renderData ? renderData : null;
  }

  default ServerDataCapable<E> getEasyNPCServerData() {
    return this instanceof ServerDataCapable<E> serverData ? serverData : null;
  }

  default StatusDataCapable<E> getEasyNPCStatusData() {
    return this instanceof StatusDataCapable<E> statusData ? statusData : null;
  }

  default TickerDataCapable<E> getEasyNPCTickerData() {
    return this instanceof TickerDataCapable<E> tickerData ? tickerData : null;
  }

  default TradingDataCapable<E> getEasyNPCTradingData() {
    return this instanceof TradingDataCapable<E> tradingData ? tradingData : null;
  }

  default SoundDataCapable<E> getEasyNPCSoundData() {
    return this instanceof SoundDataCapable<E> soundData ? soundData : null;
  }

  default VariantDataCapable<E> getEasyNPCVariantData() {
    return this instanceof VariantDataCapable<E> variantData ? variantData : null;
  }

  default ActionHandler<E> getEasyNPCActionHandler() {
    return this instanceof ActionHandler<E> actionHandler ? actionHandler : null;
  }

  default AttributeHandler<E> getEasyNPCAttributeHandler() {
    return this instanceof AttributeHandler<E> attributeHandler ? attributeHandler : null;
  }
}
